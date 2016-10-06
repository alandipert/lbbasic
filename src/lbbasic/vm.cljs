(ns lbbasic.vm
  (:refer-clojure :exclude [run!])
  (:require-macros
   [javelin.core :refer [with-let]])
  (:require
   [adzerk.cljs-console :as log :include-macros true]
   [clojure.data.avl    :as avl]
   [clojure.string      :as str]
   [lbbasic.util        :refer [peekn popn]]))

(defrecord Machine [stack               ;Operand stack
                    lines               ;AVL tree of BASIC program line numbers to vectors of instructions
                    line                ;Current line number
                    inst-ptr            ;Current instruction in the current line
                    inst-count          ;Number of instructions executed so far
                    vars                ;Map of global variables to their values
                    sleep-ms            ;If non-nil, number of milliseconds to sleep before next instruction
                    printfn             ;Function to call to print a line
                    ])

(defn new-machine
  []
  (map->Machine
   {:stack            []
    :lines            (avl/sorted-map)
    :line             nil
    :inst-ptr         0
    :inst-count       0
    :vars             {}
    :sleep-ms         nil
    :printfn          #(throw (js/Error. "printfn undefined"))}))

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

(defmethod inst :store
  [machine [_ var-name]]
  (-> machine
      (assoc-in [:vars var-name] (peek (:stack machine)))
      (update :stack pop)
      (update :inst-ptr inc)))

(defmethod inst :load
  [machine [_ var-name]]
  (if-let [val (get-in machine [:vars var-name])]
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
  [{:keys [stack printfn] :as machine} [_ argc & [separator]]]
  (let [args (peekn stack argc)]
    (printfn (str/join (or separator "") args))
    (-> machine
        (update :stack popn argc)
        (update :inst-ptr inc))))

(defmethod inst :sleep
  [{:keys [stack] :as machine} _]
  (let [ms (peek stack)]
    (-> machine
        (update :stack pop)
        (assoc :sleep-ms ms)
        (update :inst-ptr inc))))

;; Stepper

(defn step
  ([machine]
   (let [{:keys [lines line inst-ptr]} machine
         instructions                  (get lines line)]
     (if (= (count instructions) inst-ptr)
       (if-let [next-line (first (avl/nearest lines > line))]
         (recur (assoc machine :line next-line :inst-ptr 0))
         machine)
       (-> machine
           (update :inst-count inc)
           (inst (get instructions inst-ptr))))))
  ([machine n] (step machine (step machine) (dec n)))
  ([prev next n]
   (if (or (zero? n)
           (= prev next)
           ;; If the sleep-ms interrupt has a value, we need to stop computing
           ;; during this timeout so we can schedule the next one sleep-ms in
           ;; the future.
           (:sleep-ms next))
     next
     (recur next (step next) (dec n)))))

;; Runner

(defrecord VirtualMachine [machine trampoline resolve-fn reject-fn pending])

(defn make-trampoline
  ([pending machine]
   (make-trampoline pending machine 100))
  ([pending machine insts-per-timeout]
   (fn trampoline [prev resolve-fn]
     (reset! machine prev)
     (let [next (step prev 1000)]
       (if (= prev next)
         (resolve-fn prev)
         (reset! pending (.setTimeout js/window
                                      trampoline
                                      ;; If the sleep "interrupt" had a value,
                                      ;; wait that number of ms before the next
                                      ;; instruction.
                                      (or (:sleep-ms next) 0)
                                      ;; Clear the :sleep-ms interrupt
                                      (assoc next :sleep-ms nil)
                                      resolve-fn)))))))

(defn make-vm
  []
  (let [pending    (atom nil)
        resolve-fn (atom nil)
        machine    (atom (assoc (new-machine) :printfn (or printfn println)))]
    (map->VirtualMachine
     {:machine    machine
      :trampoline (make-trampoline pending machine)
      :resolve-fn resolve-fn
      :pending    pending})))

(defn load!
  [vm line instructions]
  (update vm :machine swap! load line instructions))

(defn run!
  [{:keys [machine pending resolve-fn trampoline] :as vm} line]
  (if @pending
    (throw (ex-info "VM is already running" {:vm vm}))
    (with-let [prom (js/Promise. #(reset! resolve-fn %))]
      (trampoline @machine @resolve-fn))))

(defn break!
  [{:keys [machine resolve-fn] :as vm}]
  (update vm :pending #(.clearTimeout js/window @%))
  (@resolve-fn @machine))

(defn doit
  []
  (let [vm (make-vm)]
    (load! vm 10
           [[:push 2]
            [:push 1]
            [:lt]
            [:ifjmp 4]
            [:push 2]
            [:print 1]
            [:jmp 3]
            [:push 1]
            [:print 1]]
           #_
           [[:push 0]
            [:store "x"]
            [:load "x"]
            [:push 1]
            [:lt]
            [:ifjmp 4]
            [:push 2]
            [:store "x"]
            [:jmp 3]
            [:push 123]
            [:print 1]]
           #_[[:push 1]
                  [:push 2]
                  [:push 3]
                  [:push 4]
                  [:sub]
                  [:div]
                  [:sub]
                  [:push 5]
                  [:push 6]
                  [:mul]
                  [:add]
                  [:print 1]])
    ;; (load! vm 10 [[:push 0] [:store "i"]])
    ;; (load! vm 11 [[:push "got here, sleeping 1000ms"] [:print 1]])
    ;; (load! vm 12 [[:push 1000] [:sleep]])
    ;; (load! vm 13 [[:push "done sleeping!"] [:print 1]])
    ;; (load! vm 15 [[:load "i"] [:push 1] [:+] [:store "i"]])
    ;; (load! vm 16 [[:push "got here, sleeping 1000ms"] [:print 1]])
    ;; (load! vm 17 [[:push 1000] [:sleep]])
    ;; (load! vm 20 [[:goto 15]])
    (.then (run! vm 10) #(println "insts=" (:inst-count %)
                                  "i=" (get-in % [:vars "i"])))
    ;; (.setTimeout js/window break! 5000 vm)
    ))
