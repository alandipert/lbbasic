(ns lbbasic.util)

(defn peekn
  [v n]
  (subvec v (- (count v) n) (count v)))

(defn popn
  [v n]
  (subvec v 0 (- (count v) n)))

(defn concatv
  [& xs]
  (vec (apply concat xs)))

(def default-queue (atom cljs.core/PersistentQueue.EMPTY))

(defn- handle [q]
  (let [[not-before thunk] (peek @q)
        now                (.getTime (js/Date.))]
    (if (<= not-before now)
      (do (swap! q pop) (thunk))
      (.setTimeout js/window #(handle q) (- not-before now)))))

(defn after
  "Enqueues a function for invocation after some number of ms. The function is
  not invoked until after all previously enqueued functions have been invoked."
  ([ms f] (after default-queue ms f))
  ([q ms f]
   (let [not-before (+ (.getTime (js/Date.)) ms)]
     (swap! q conj [not-before f])
     (.setTimeout js/window #(handle q) ms))))

