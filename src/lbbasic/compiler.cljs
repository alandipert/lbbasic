(ns lbbasic.compiler
  (:require [instaparse.core :as insta]
            [adzerk.cljs-console :as log :include-macros true]
            [cljs.core.match :refer-macros [match]]))

(def parse
  (insta/parser
   ;;{{
   line         = (linum ws+)? stmt
   linum        = #'(0|([1-9][0-9]*))'
   <stmt>       = assign | builtin
   (* assignment *)
   assign       = var ws* <'='> ws* expr
   (* builtins *)
   builtin      = print
   print        = <'print'> ws* expr
   (* arithmethic *)
   <expr>       = add-sub | value | var
   <add-sub>    = mul-div | add | sub
   add          = add-sub ws* <'+'> ws* mul-div
   sub          = add-sub ws* <'-'> ws* mul-div
   <mul-div>    = term | mul | div
   mul          = mul-div ws* <'*'> ws* term
   div          = mul-div ws* <'/'> ws* term
   <term>       = float | int | var | <'('> ws* add-sub ws* <')'>
   (* literals *)
   <value>      = string | float | int
   string       = #'\"[^\"]+\"'
   float        = #'[+-]?(0|([1-9][0-9]*))(\.[0-9]+)'
   int          = #'[+-]?(0|([1-9][0-9]*))'
   (* variable reference *)
   var          = #'[a-zA-Z]+'
   (* util *)
   <ws> = <#'\s'>
   ;;}}
   ))

(defn linum
  [parsed]
  (match parsed [:line [:linum n]] n :else nil))

(defn statement
  [parsed]
  (match parsed
    [:line [:linum _] prog] prog
    [:line prog] prog
    :else (throw (ex-info "Unknown parse result" {:parsed parsed}))))

(defn concatv [& xs] (vec (apply concat xs)))

(defn compile
  [prog]
  (match prog
    [:builtin [:print x]]
    (conj (compile x) [:print 1])
    [:int i]
    [[:push (js/parseInt i)]]
    [:add x y]
    (concatv (compile x) (compile y) [[:+]])
    [:sub x y]
    (concatv (compile x) (compile y) [[:-]])
    [:div x y]
    (concatv (compile x) (compile y) [[:/]])
    [:mul x y]
    (concatv (compile x) (compile y) [[:*]])
    [:assign [:var var-name] expr]
    (conj (compile expr) [:store var-name])
    :else
    (throw (ex-info "Don't know how to compile" {:prog prog}))))

(defn doit
  []
  #_(println (compile (statement (parse "10 print 1+2+3"))))
  (println (compile (statement (parse "10 print 1-2/(3-4)+5*6"))))
  #_(js/alert "did it")
  #_(println (linum [:line [:foo 321]])))
