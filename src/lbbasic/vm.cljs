(ns lbbasic.vm
  (:refer-clojure :exclude [run!])
  (:require-macros [javelin.core :refer [with-let]])
  (:require [clojure.data.avl :as avl]
            [lbbasic.util :refer [peekn popn]]
            [adzerk.cljs-console :as log :include-macros true]
            [clojure.string :as str]))

(defrecord Machine [stack lines line inst-ptr fors vars printfn])

(defn new-machine
  []
  (map->Machine
   {:stack           []
    :lines           (avl/sorted-map)
    :line            nil
    :inst-ptr nil
    :fors            []
    :vars            {}
    :printfn         #(throw (ex-info "printfn undefined" {}))}))

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

(defmethod inst :dup
  [machine _]
  (-> machine
      (update :stack conj (peek (:stack machine)))
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

(defmethod inst :+
  [machine _]
  (let [[x y :as args] (peekn (:stack machine) 2)
        ret            (condp = (mapv type args)
                         [js/String js/String]
                         (str x y)
                         [js/Number js/Number]
                         (+ x y)
                         :else (throw (ex-info "plus: unknown argument types"
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

(defmethod inst :- [machine _] (op-numeric - "minus" machine))
(defmethod inst :* [machine _] (op-numeric - "times" machine))
(defmethod inst :/ [machine _] (op-numeric - "divide" machine))
(defmethod inst :% [machine _] (op-numeric - "mod" machine))

;; Numeric comparisons

(defn bool [x] (if x 1 0))

(defn compare-numeric
  [compare-fn {:keys [stack] :as machine}]
  (let [[x y] (peekn stack 2)]
    (-> machine
        (update :stack popn 2)
        (update :stack conj (bool (compare-fn x y)))
        (update :inst-ptr inc))))

(defmethod inst :<  [machine _] (compare-numeric < machine))
(defmethod inst :<= [machine _] (compare-numeric <= machine))
(defmethod inst :>  [machine _] (compare-numeric > machine))
(defmethod inst :>= [machine _] (compare-numeric >= machine))

;; I/O

(defmethod inst :print
  [{:keys [stack printfn] :as machine} [_ argc & [separator]]]
  (let [args (peekn stack argc)]
    (printfn (str/join (or separator "") args))
    (-> machine
        (update :stack popn argc)
        (update :inst-ptr inc))))

(defn step1
  [run-state debug? machine]
  (let [{:keys [line inst-ptr]} machine
        instructions            (get-in machine [:lines line])]
    (when debug?
      (.debug js/console (str "stack: " (pr-str (:stack machine))))
      (.debug js/console (str "instr: " line " " inst-ptr " " (get-in machine [:lines line inst-ptr]))))
    (if (= (count instructions) inst-ptr)
      (if-let [next-line (first (avl/nearest (:lines machine) > line))]
        (assoc machine :line next-line :inst-ptr 0)
        machine)
      (inst machine (get instructions inst-ptr)))))

(defn stepN
  ([run-state debug? machine pipeline-size]
   (stepN run-state debug? machine (step1 run-state debug? machine) 1 (dec pipeline-size)))
  ([run-state debug? prev-machine machine inst-count pipeline-size]
   (cond (zero? pipeline-size)
         (do (swap! run-state update :inst-count + inst-count)
             machine)
         (= prev-machine machine)
         (do (swap! run-state update :inst-count + (dec inst-count))
             machine)
         :else (recur run-state debug? machine (step1 run-state debug? machine) (inc inst-count) (dec pipeline-size)))))

(defrecord RunState [machine running? debug? started-at inst-count halted-fn stopped-fn])
(defrecord RunningMachine [run stop load])

(defn date-minus
  [d1 d2]
  (- (.valueOf d1) (.valueOf d2)))

(defn make-end-status
  [end-state]
  (let [elapsed (date-minus (js/Date.) (:started-at end-state))]
    {:elapsed      elapsed
     :inst-count   (:inst-count end-state)
     :inst-per-sec (/ (:inst-count end-state) (/ elapsed 1000))}))

;; TODO save prev version of machine to support STOP/CONTINUE
(defn make-trampoline
  [run-state debug? interval pipeline-size]
  (fn trampoline [prev]
    (let [{:keys [halted-fn stopped-fn]} @run-state
          next                           (stepN run-state debug? prev pipeline-size)]
      (cond (= prev next)
            (do (halted-fn (make-end-status @run-state))
                (swap! run-state assoc :running? false))
            (not (:running? @run-state))
            (stopped-fn (make-end-status @run-state))
            :else (.setTimeout js/window trampoline interval next)))))

;; TODO clean up the separation between the idea of a machine and a running
;; machine. Maybe use new bound-fn to propagate config flags dynamically? Vs.
;; threading through many functions.
(defn make-vm
  ([] (make-vm (new-machine)))
  ([init-machine]
   (let [run-state (atom (map->RunState
                          {:machine    init-machine
                           :running?   false
                           :debug?     false
                           :started-at nil
                           :inst-count 0
                           :halted-fn  nil
                           :stopped-fn nil}))]
     (map->RunningMachine
      {:state (:machine @run-state)
       :run   (fn run*
                ([] (run* {}))
                ([opts]
                 (if-let [first-line (first (keys (-> @run-state :machine :lines)))]
                   (run* first-line opts)
                   (log/error "No lines loaded")))
                ([line {:keys [debug?
                               interval
                               pipeline-size
                               printfn
                               profile?
                               profile-name]
                        :or   {debug?        false
                               interval      0
                               pipeline-size 20
                               printfn       #(.log js/console %)
                               profile?      false
                               profile-name  (str (js/Date.))}
                        :as   opts}]
                 (if (:running? @run-state)
                   (throw (js/Error. "VM is already running"))
                   (let [trampoline (make-trampoline run-state debug? interval pipeline-size)]
                     (with-let [prom (js/Promise.
                                      (fn [resolve reject]
                                        (swap! run-state assoc :halted-fn
                                               #(do (if profile? (.profileEnd js/console))
                                                    (resolve %)))
                                        (swap! run-state assoc :stopped-fn
                                               #(do (if profile? (.profileEnd js/console))
                                                    (reject %)))))]
                       (if profile? (.profile js/console profile-name))
                       (swap! run-state assoc :started-at (js/Date.))
                       (swap! run-state assoc :running? true)
                       (swap! run-state update :machine merge
                              {:line     line
                               :inst-ptr 0
                               :printfn  printfn})
                       (trampoline (:machine @run-state)))))))
       :stop  (fn [] (swap! run-state assoc :running? false))
       :load  (fn [line instructions]
                (swap! run-state assoc-in [:machine :lines line] instructions))}))))

(defn load-line!
  [vm line insts]
  ((:load vm) line insts)
  vm)

(defn load-program!
  [vm prog]
  (doseq [[line insts] prog] (load-line! vm line insts))
  vm)

(defn run!
  [vm & args]
  (apply (:run vm) args))

(defn stop!
  [vm]
  ((:stop vm))
  vm)
