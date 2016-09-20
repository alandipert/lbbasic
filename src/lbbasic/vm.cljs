(ns lbbasic.vm
  (:require [clojure.data.avl :as avl]
            [adzerk.cljs-console :as log :include-macros true]))

(defrecord Machine
    [stack lines line instruction fors vars printfn])

(defn make-machine
  [{:keys [printfn]}]
  (map->Machine
   {:stack        []
    :lines        (avl/sorted-map)
    :line         nil
    :instruction  nil
    :fors         []
    :vars         {}
    :printfn      printfn}))

(defn load
  ([machine line instructions]
   (update machine :lines assoc line instructions)))

(defmulti eval (fn [machine [op & args]] op))

(defmethod eval :push
  [machine [_ val]]
  (-> machine
      (update :stack conj val)
      (update :instruction inc)))

(defmethod eval :dup
  [machine _]
  (-> machine
      (update :stack conj (peek (:stack machine)))
      (update :instruction inc)))

(defmethod eval :store
  [machine [_ var-name]]
  (-> machine
      (assoc-in [:vars var-name] (peek (:stack machine)))
      (update :stack pop)
      (update :instruction inc)))

(defmethod eval :load
  [machine [_ var-name]]
  (if-let [val (get-in machine [:vars var-name])]
    (-> machine
        (update :stack conj val)
        (update :instruction inc))
    (throw (ex-info "Undefined var" {:name var-name
                                     :line (:line machine)}))))

(defmethod eval :goto
  [machine [_ goto-line]]
  (if (contains? (:lines machine) goto-line)
    (merge machine {:line goto-line :instruction 0})
    (throw (ex-info "goto: goto-line doesn't exist" {:line (:line machine)
                                                     :goto goto-line}))))

(defn peekn [v n]
  (subvec v (- (count v) n) (count v)))

(defn popn [v n]
  (subvec v 0 (- (count v) n)))

(defmethod eval :plus
  [machine _]
  (let [[x y :as args] (peekn (:stack machine) 2)
        ret            (condp = (mapv type args)
                         [js/String js/String]
                         (str x y)
                         [js/Number js/Number]
                         (+ x y))]
    (-> machine
        (update :stack #(conj (popn % 2) ret))
        (update :instruction inc))))

(defmethod eval :print
  [machine _]
  (println (peek (:stack machine)))
  (-> machine
      (update :stack pop)
      (update :instruction inc)))

(defn run1
  [machine]
  (let [{:keys [line instruction]} machine
        instructions               (get-in machine [:lines line])]
    (log/debug "#{line} #{instruction} ~(get-in machine [:lines line instruction 0])")
    (if (= (count instructions) instruction)
      (if-let [next-line (first (avl/nearest (:lines machine) > line))]
        (assoc machine :line next-line :instruction 0)
        machine)
      (eval machine (get instructions instruction)))))

;; TODO roll this into a "make-runner" function that sets up the atoms,
;; break/pause callbacks
(def break (atom nil))
(def last-machine (atom nil))
(defn run
  ([machine line]
   (run (merge machine {:line line :instruction 0})))
  ([machine]
   (reset! last-machine machine)
   (.setTimeout js/window
                #(let [prev-machine @last-machine
                       next-machine (run1 @last-machine)]
                   (if (not= prev-machine next-machine)
                     (run next-machine)
                     (log/info "Done")))
                0)))

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
