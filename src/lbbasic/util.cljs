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

