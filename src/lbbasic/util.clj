(ns lbbasic.util)

(defmacro cond-let [& clauses]
  (when-let [[binding expr & clauses] (seq clauses)]
    (if (vector? binding)
      `(if-let ~binding ~expr (cond-let ~@clauses))
      ;; To support :else
      expr)))
