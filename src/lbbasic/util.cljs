(ns lbbasic.util
  (:require [javelin.core :refer [dosync]]))

(defn peekn
  [v n]
  (subvec v (- (count v) n) (count v)))

(defn popn
  [v n]
  (subvec v 0 (- (count v) n)))

(defn concatv
  [& xs]
  (vec (apply concat xs)))

(defn resets!
  "Like reset!, but takes any number of atom/value pairs and resets them all a la setq"
  [& atm-vals]
  (dosync
   (doseq [[atm val] (partition 2 atm-vals)]
     (reset! atm val))))

