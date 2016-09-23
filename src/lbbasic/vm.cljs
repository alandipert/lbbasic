(ns lbbasic.vm
  (:refer-clojure :exclude [run!])
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
  [machine]
  (let [{:keys [line inst-ptr]} machine
        instructions            (get-in machine [:lines line])]
    (log/debug "stack: ~(:stack machine)")
    (log/debug "instr: #{line} #{inst-ptr} ~(get-in machine [:lines line inst-ptr])")
    (if (= (count instructions) inst-ptr)
      (if-let [next-line (first (avl/nearest (:lines machine) > line))]
        (assoc machine :line next-line :inst-ptr 0)
        machine)
      (inst machine (get instructions inst-ptr)))))

(defn stepN
  ([machine pipeline-size]
   (stepN machine (step1 machine) (dec pipeline-size)))
  ([prev-machine machine pipeline-size]
   (if (or (zero? pipeline-size) (= prev-machine machine))
     machine
     (recur machine (step1 machine) (dec pipeline-size)))))

(defrecord VirtualMachine [run stop load])

(defn make-vm
  ([opts] (make-vm (new-machine) opts))
  ([init-machine {:keys [interval
                         pipeline-size
                         printfn]
                  :or   {interval      0
                         pipeline-size 1}
                  :as   opts}]
   (let [machine  (atom (assoc init-machine
                               ;; Initialize machine with user-supplied output
                               ;; functions.
                               :printfn printfn))
         running? (atom false)]
     (letfn [(trampoline [prev]
               (let [next (stepN prev pipeline-size)]
                 (cond (= prev next)
                       (do (log/info "Halted")
                           (reset! running? false))
                       (not @running?)
                       (log/info "Stopped")
                       :else (.setTimeout js/window trampoline interval next))))]
       (map->VirtualMachine
        {:run  (fn run*
                 ([]
                  (if-let [first-line (first (keys (:lines @machine)))]
                    (run* first-line)
                    (log/error "No lines loaded")))
                 ([line]
                  (reset! running? true)
                  (swap! machine assoc :line line :inst-ptr 0)
                  (trampoline @machine)))
         :stop (fn [] (reset! running? false))
         :load (fn [line instructions]
                 (swap! machine assoc-in [:lines line] instructions))})))))

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
  (apply (:run vm) args)
  vm)

(defn stop!
  [vm]
  ((:stop vm))
  vm)

;; 10 FOR X = 0 TO 10
;; 20 PRINT X
;; 30 NEXT
;; 40 GOTO 20

[[:line 10]
 [:push 0]                              ;initial
 [:push 10]                             ;max
 [:push 1]                              ;step-by
 [:for "X"]                             ;push for on fors
 [:line 20]
 [:load "X"]                            ;push X on stack
 [:print]                               ;print/pop top of stack
 [:line 30]
 [:next]                                ;loop to nearest for (X)
 ]

;; [[:label 10 0]
;;  [:for "X"]
;;  [:push 0]
;;  [:store "X"]
;;  [:label 10 1]
;;  [:load "X"]
;;  [:push 10]
;;  [:lt]
;;  [:ifngoto 10 2]
;;  [:load "X"]
;;  [:print]
;;  [:label 10 2]
;;  [:next "X"]]

;; 10 X = 0
;; 20 IF X > 10 THEN GOTO 60
;; 30 PRINT X
;; 40 X = X + 1
;; 50 GOTO 20
;; 60 END

[[:label 10 0]                           ;current-line 10 label 0
 [:const 1]                              ;push 1 on the stack
 [:store "X"]                            ;store top of stack as variable I
 [:pop]                                  ;pop 1 off the stack
 [:label 10 1]                           ;current-line 10 label 1
 [:load "X"]                             ;push value of variable I on stack
 [:const 1]                              ;push 1 on the stack
 [:add]                                  ;pop I and 1 and push I + 1
 [:store "X"]                           ;
 [:load "X"]
 [:const 10]
 [:lt]
 [:ifgoto 10 1]]
