(ns lbbasic.vm
  (:refer-clojure :exclude [run!])
  (:require-macros
   [javelin.core :refer [with-let]]
   [clojure.core.strint :refer [<<]]
   [lbbasic.util        :refer [cond-let]])
  (:require
   [adzerk.cljs-console :as log :include-macros true]
   [clojure.data.avl    :as avl]
   [clojure.string      :as str]
   [lbbasic.util        :refer [after peekn popn]]
   [lbbasic.messages    :as msg]
   [cognitect.transit   :as t]))

(defrecord Machine [stack               ;Operand stack
                    lines               ;AVL tree of BASIC program line numbers to vectors of instructions
                    line                ;Current line number
                    inst-ptr            ;Current instruction in the current line
                    vars                ;Map of global variables to their values
                    printfn             ;Function to call to print a line.  Should only have a value in the Worker.
                    ])

(defn new-machine
  []
  (map->Machine
   {:stack            []
    :lines            (avl/sorted-map)
    :line             nil
    :inst-ptr         0
    :vars             {}
    :printfn          nil}))

(let [w (t/writer :json)
      r (t/reader :json)]
  (def cljs->js (partial t/write w))
  (def js->cljs (partial t/read r)))

(defn serialize-machine
  "Prepares a machine for transfer to a web worker."
  [machine]
  (-> (into {} machine)
      (assoc :printfn nil)
      (update :lines (partial into {}))
      cljs->js))

(defn deserialize-machine
  "Reconstructs a machine from a web worker message."
  [machine]
  (-> (js->cljs machine)
      (update :lines (partial into (avl/sorted-map)))))

(defn load
  ([machine line instructions]
   (update machine :lines assoc line instructions)))

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
  [{:keys [stack printfn] :as machine} [_ argc separator]]
  (let [args (peekn stack argc)]
    (printfn (str/join separator args))
    (-> machine
        (update :stack popn argc)
        (update :inst-ptr inc))))

;; Stepper

(defn step
  ([machine]
   (let [{:keys [lines line inst-ptr]} machine
         instructions                  (get lines line)]
     (if (= (count instructions) inst-ptr)
       ;; Line -1 is used to store the "immediate" line. Don't proceed to the
       ;; next line automatically if the current line is -1.
       (if (= line -1)
         machine
         (if-let [next-line (first (avl/nearest lines > line))]
           (recur (assoc machine :line next-line :inst-ptr 0))
           machine))
       (inst machine (get instructions inst-ptr)))))
  ([machine n] (step machine (step machine) (dec n)))
  ([prev next n]
   (if (or (zero? n) (= prev next))
     next
     (recur next (step next) (dec n)))))

;; Runner

(defrecord VirtualMachine [machine worker resolve-fn reject-fn])

(defn make-vm
  [{:keys [printfn] :as opts}]
  (let [resolve-fn (atom nil)
        machine    (atom (new-machine))
        worker     (js/Worker. "vm_worker.js")]
    (set! (.-onmessage worker)
          (fn [msg]
            (case (.. msg -data -type)
              "halt"  (let [last-machine (deserialize-machine (.. msg -data -machine))]
                        (reset! machine last-machine)
                        (@resolve-fn last-machine))
              "break" (let [last-machine (deserialize-machine (.. msg -data -machine))]
                        (reset! machine last-machine)
                        (printfn (<< "Break on line ~(:line last-machine), instruction ~(:inst-ptr last-machine)"))
                        (@resolve-fn last-machine))
              "print" (printfn (.. msg -data -line))
              (throw (ex-info "Unknown message type received from worker"
                              {:type (.. msg -data -type)
                               :msg  msg})))))
    (set! (.-onerror worker)
          (fn [e]
            (if (pos? (.indexOf (.-message e) "#error"))
              (do (.preventDefault e)
                  (printfn (.-message e))))))
    (map->VirtualMachine
     {:machine    machine
      :worker     worker
      :resolve-fn resolve-fn})))

(defn load!
  [vm line instructions]
  (update vm :machine swap! load line instructions))

(defn run!
  [{:keys [machine worker resolve-fn] :as vm} line]
  (with-let [prom (js/Promise. #(reset! resolve-fn %))]
    (msg/send! worker :run #js{"machine" (serialize-machine @machine)
                               "line"    line})))

(defn break!
  [{:keys [worker] :as vm}]
  (msg/send! worker :interrupt #js{"name" "break" "value" true}))
