(ns lbbasic.util)

(defmacro cond-let [& clauses]
  (when-let [[binding expr & clauses] (seq clauses)]
    `(if-let ~binding ~expr (cond-let ~@clauses))))
