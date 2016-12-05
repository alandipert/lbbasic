(ns lbbasic.vm
  (:refer-clojure :exclude [run!])
  (:require-macros
   [javelin.core :refer [with-let cell=]]
   [clojure.core.strint :refer [<<]])
  (:require
   [adzerk.cljs-console :as log :include-macros true]
   [clojure.data.avl    :as avl]
   [clojure.string      :as str]
   [lbbasic.util        :refer [after peekn popn]]
   [javelin.core        :refer [cell]]))

(defrecord Machine [stack               ;Operand stack
                    lines               ;AVL tree of BASIC program line numbers to vectors of instructions
                    source              ;Sorted map of line number to source code string
                    line                ;Current line number
                    inst-ptr            ;Current instruction in the current line
                    vars                ;Map of global variables to their values
                    interrupt           ;If set: [:interrupt.name <val>]
                    ])

(defn new-machine
  []
  (map->Machine
   {:stack     []
    :lines     (avl/sorted-map)
    :source    (sorted-map)
    :line      nil
    :inst-ptr  0
    :vars      {}
    :interrupt nil}))

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

(def special-variables
  {"time" #(.getTime (js/Date.))})

(defmethod inst :store
  [machine [_ var-name]]
  (if (contains? special-variables var-name)
    (throw (ex-info "can't set special var" {:name var-name
                                             :line (:line machine)}))
    (-> machine
        (assoc-in [:vars var-name] (peek (:stack machine)))
        (update :stack pop)
        (update :inst-ptr inc))))

(defmethod inst :load
  [machine [_ var-name]]
  (if-let [val (if-let [special-var (get special-variables var-name)]
                 (special-var)
                 (get-in machine [:vars var-name]))]
    (-> machine
        (update :stack conj val)
        (update :inst-ptr inc))
    (throw (ex-info "undefined var" {:name var-name
                                     :line (:line machine)}))))

;; Flow control

(defn truth [x] (not= x 0))

(defmethod inst :goto
  [machine [_ goto-line]]
  (if (contains? (:lines machine) goto-line)
    (assoc machine :line goto-line :inst-ptr 0)
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
  ([machine n] (step machine (step machine) (dec n)))
  ([prev next n]
   (if (or (zero? n)
           (= prev next)
           (:interrupt next))
     next
     (recur next (step next) (dec n)))))

;; Runner

(defrecord VirtualMachine [machine
                           pending-timeout
                           running?
                           resolve-fn
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
  (let [machine         (atom (new-machine))
        pending-timeout (cell nil)
        running?        (cell= (boolean pending-timeout))
        resolve-fn      (atom nil)]
    (map->VirtualMachine
     {:machine         machine
      :pending-timeout pending-timeout
      :running?        running?
      :resolve-fn      resolve-fn
      :printfn         printfn})))

(defn load!
  [{:keys [running?] :as vm} line source instructions]
  (when @running? (throw (js/Error. "Can't load instructions, VM is currently running.")))
  (update vm :machine swap! load line source instructions))

(defn run!
  [{:keys [machine running? pending-timeout resolve-fn printfn] :as vm} line]
  (when @running? (throw (js/Error. "Can't run VM, it's already running.")))
  (with-let [prom (js/Promise. #(reset! resolve-fn %))]
    (letfn [(tramp []
              (let [prev @machine
                    next (step prev 10)]
                (if (= :break (get-in next [:interrupt 0]))
                  ;; [:break] interrupt is special because we need to stop the trampoline.
                  (do (reset! pending-timeout nil)
                      (@resolve-fn {:msg :break :machine prev}))
                  ;; Other kinds of interrupt are handled here.
                  (let [next-handled (handle-clear-interrupt vm next)]
                    (if (= next-handled prev)
                      (do (reset! pending-timeout nil)
                          (@resolve-fn {:msg :halted :machine prev}))
                      (do (reset! machine next-handled)
                          (reset! pending-timeout (js/setTimeout tramp 0))))))))]
      (swap! machine merge {:line line :inst-ptr 0})
      (reset! pending-timeout (js/setTimeout tramp 0)))))

(defn break!
  [{:keys [worker] :as vm}]
  (with-let [vm vm]
    (swap! vm update assoc :interrupt [:break true])))
