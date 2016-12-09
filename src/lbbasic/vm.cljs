(ns lbbasic.vm
  (:refer-clojure :exclude [run!])
  (:require [clojure.data.avl :as avl]
            [clojure.string :as str]
            [javelin.core :refer [cell]]
            [lbbasic.util :refer [peekn popn resets!]])
  (:require-macros
   [hoplon.core :refer [with-timeout]]
   [javelin.core :refer [dosync]]))

(defrecord Machine [stack               ;Operand stack
                    lines               ;AVL tree of BASIC program line numbers to vectors of instructions
                    source              ;Sorted map of line number to source code string
                    line                ;Current line number
                    goto-flipper        ;Hack to make 10 goto 10 work
                    inst-ptr            ;Current instruction in the current line
                    vars                ;Map of global variables to their values
                    interrupt           ;If set: [:interrupt.name <val>]
                    ])

(defn new-machine
  []
  (map->Machine
   {:stack        []
    :lines        (avl/sorted-map)
    :source       (sorted-map)
    :line         nil
    :goto-flipper true
    :inst-ptr     0
    :vars         {}
    :interrupt    nil}))

(defn load
  ([machine line source instructions]
   (-> machine
       (update :lines assoc line instructions)
       (update :source assoc line source))))

;; VM instructions

(defmulti inst (fn [machine [op & args]] op))

;; Basic stack manipulation

(defmethod inst :push
  [machine [_ val]]
  (-> machine
      (update :stack conj val)
      (update :inst-ptr inc)))

;; Setting/getting variables

(defmulti get-variable (fn [machine var-name] var-name))

(defmethod get-variable "time"
  [machine _]
  (.getTime (js/Date.)))

(defmethod get-variable :default
  [machine var-name]
  (get-in machine [:vars var-name]))

(defmethod inst :store
  [machine [_ var-name]]
  (if (contains? (methods get-variable) var-name)
    (throw (ex-info "can't set special var" {:name var-name :line (:line machine)}))
    (-> machine
        (assoc-in [:vars var-name] (peek (:stack machine)))
        (update :stack pop)
        (update :inst-ptr inc))))

(defmethod inst :load
  [machine [_ var-name]]
  (if-let [val (get-variable machine var-name)]
    (-> machine
        (update :stack conj val)
        (update :inst-ptr inc))
    (throw (ex-info "undefined var" {:name var-name :line (:line machine)}))))

;; Flow control

(defn truth [x] (not= x 0))

(defmethod inst :goto
  [machine [_ goto-line]]
  (if (contains? (:lines machine) goto-line)
    (-> machine
        ;; Makes 10 goto 10 possible
        (update :goto-flipper not)
        (assoc :line goto-line :inst-ptr 0))
    (throw (ex-info "goto: goto-line doesn't exist"
                    {:line (:line machine) :goto goto-line}))))

(defmethod inst :ifjmp
  [machine [_ n]]
  {:pre [(> n 0)]}
  (if (truth (peek (:stack machine)))
    (-> machine
        (update :stack pop)
        (update :inst-ptr + n))
    (-> machine
        (update :stack pop)
        (update :inst-ptr inc))))

(defmethod inst :jmp
  [machine [_ n]]
  {:pre [(> n 0)]}
  (update machine :inst-ptr + n))

;; Functions

(defmethod inst :add
  [machine _]
  (let [[x y :as args] (peekn (:stack machine) 2)
        ret            (condp = (mapv type args)
                         [js/String js/String]
                         (str x y)
                         [js/Number js/Number]
                         (+ x y)
                         :else (throw (ex-info "add: unknown argument types"
                                               {:types (mapv type args)
                                                :line (:line machine)})))]
    (-> machine
        (update :stack #(conj (popn % 2) ret))
        (update :inst-ptr inc))))

;; Numeric functions

(defn op-numeric
  [op-fn op-name {:keys [stack] :as machine}]
  (let [[x y :as args] (peekn (:stack machine) 2)
        ret            (condp = (mapv type args)
                         [js/Number js/Number]
                         (op-fn x y)
                         :else (throw (ex-info (str op-name ": argument(s) not numeric")
                                               {:types (mapv type args)
                                                :line (:line machine)})))]
    (-> machine
        (update :stack #(conj (popn % 2) ret))
        (update :inst-ptr inc))))

(defmethod inst :sub [machine _] (op-numeric - "subtract" machine))
(defmethod inst :mul [machine _] (op-numeric * "multiply" machine))
(defmethod inst :div [machine _] (op-numeric / "divide" machine))
(defmethod inst :mod [machine _] (op-numeric mod "mod" machine))

;; Equality

(defmethod inst :eq
  [{:keys [stack] :as machine}]
  (-> machine
      (update :stack popn 2)
      (update :stack conj (apply = (peekn stack 2)))
      (update :inst-ptr inc)))

;; Numeric comparisons

(defn bool [x] (if x 1 0))

(defn compare-numeric
  [compare-fn {:keys [stack] :as machine}]
  (let [[x y] (peekn stack 2)]
    (-> machine
        (update :stack popn 2)
        (update :stack conj (bool (compare-fn x y)))
        (update :inst-ptr inc))))

(defmethod inst :lt  [machine _] (compare-numeric < machine))
(defmethod inst :lte [machine _] (compare-numeric <= machine))
(defmethod inst :gt  [machine _] (compare-numeric > machine))
(defmethod inst :gte [machine _] (compare-numeric >= machine))

;; I/O

(defmethod inst :print
  [{:keys [stack] :as machine} [_ argc separator]]
  (let [args (peekn stack argc)]
    (-> machine
        (assoc :interrupt [:print [(str/join separator args)]])
        (update :stack popn argc)
        (update :inst-ptr inc))))

;; Reflection and interpreter state

(defmethod inst :list
  [{:keys [source] :as machine} [_ from-line to-line]]
  (let [source-lines (for [[line text] source :when (not= line -1)] text)]
    (-> machine
        (assoc :interrupt [:print source-lines])
        (update :inst-ptr inc))))

;; Stepper

(defn step
  ([machine]
   (let [{:keys [lines line inst-ptr]} machine
         instructions                  (get lines line)]
     (if (= (count instructions) inst-ptr)
       ;; Line -1 is special: it's used to store the "immediate" line. Don't
       ;; proceed to the next line automatically if the current line is -1.
       (if (= line -1)
         machine
         (if-let [next-line (first (avl/nearest lines > line))]
           (recur (assoc machine :line next-line :inst-ptr 0))
           machine))
       (inst machine (get instructions inst-ptr)))))
  ;; Secret arity
  ;; We don't check whether we're done in this loop, so there will be up to n-1
  ;; possibly redundant steps performed. Oh well. On the plus side, the logic
  ;; for stopping lives in only one place, where this function is first called.
  ([machine _ n]
   (if (or (zero? n) (:interrupt machine))
     machine
     (recur (step machine) nil (dec n))))
  ([machine n]
   ;; It's important to step an odd number of times in support of
   ;; the :goto-flipper.
   (step machine nil (if (even? n) (inc n) n))))

;; Runner

(defrecord VirtualMachine [machine
                           user-break
                           pending-timeout
                           state
                           printfn])

;; Interrupt handling

(defmulti handle-interrupt (fn [vm machine [interrupt-name val]] interrupt-name))

(defmethod handle-interrupt :print
  [{:keys [printfn] :as vm} machine [_ lines :as interrupt]]
  (doseq [line lines] (printfn line)))

(defn handle-clear-interrupt
  [vm {:keys [interrupt] :as machine}]
  (if interrupt
    (do (handle-interrupt vm machine interrupt)
        (assoc machine :interrupt nil))
    machine))

(defn make-vm
  [{:keys [printfn]
    :or {printfn (.. js/window -console -log (bind (js* "this")))}
    :as opts}]
  (let [machine         (cell (new-machine))
        user-break      (atom nil)
        pending-timeout (cell nil)
        ;;one of :stop, :run, :break
        ;; TODO: :error state, show errors
        state           (cell :stop)]
    (map->VirtualMachine
     {:machine         machine
      :user-break      user-break
      :pending-timeout pending-timeout
      :state           state
      :printfn         printfn})))

(defn clear!
  [{:keys [state] :as vm}]
  (when (= @state :run) (throw (js/Error. "Can't load instructions, VM is currently running.")))
  (update vm :machine reset! (new-machine)))

(defn load!
  [{:keys [state] :as vm} line source instructions]
  (when (= @state :run) (throw (js/Error. "Can't load instructions, VM is currently running.")))
  (update vm :machine swap! load line source instructions))

(def +insts-per-timeout+ 10)

(defn run!
  [{:keys [machine user-break state pending-timeout printfn] :as vm} line]
  (when (= @state :run) (throw (js/Error. "Can't run VM, it's already running.")))
  (letfn [(tramp [prev]
            (if @user-break
              (dosync (js/clearTimeout @pending-timeout)
                      (resets! user-break false
                               pending-timeout nil
                               machine (handle-clear-interrupt vm prev)
                               state :break))
              (let [prev-handled (handle-clear-interrupt vm prev)
                    next         (step prev-handled +insts-per-timeout+)]
                (if (= prev-handled next)
                  (dosync (js/clearTimeout @pending-timeout)
                          (resets! pending-timeout nil
                                   machine next
                                   state :stop))
                  (dosync (resets! machine next
                                   pending-timeout (js/setTimeout #(tramp next) 0)))))))]
    (dosync
     (resets! pending-timeout (with-timeout 0
                                (as-> (new-machine) |
                                  (select-keys | [:stack :line :inst-ptr])
                                  (merge @machine |)
                                  (tramp |)))
              state :run))))

(defn break!
  [{:keys [state] :as vm}]
  (when (= @state :run) (update vm :user-break reset! true)))

(defn run-now!
  [{:keys [state] :as vm} source instructions]
  (when (= @state :run) (throw (js/Error. "Can't run immediate code, VM already running.")))
  (doto vm
    (load! -1 source instructions)
    (run! -1)))
