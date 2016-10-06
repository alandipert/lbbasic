(ns lbbasic.compiler
  (:require [instaparse.core :as insta]
            [adzerk.cljs-console :as log :include-macros true]
            [cljs.core.match :refer-macros [match]]))

(def parse
  (insta/parser
   ;;{{
   line         = (linum ws+)? stmt
   linum        = #'(0|([1-9][0-9]*))'
   <stmt>       = assign | if | builtin
   (* conditionals *)
   if            = <'if'> ws+ expr ws+ <'then'> ws+ stmt ws+ (<'else'> ws+ stmt)?
   (* assignment *)
   assign       = var ws* <'='> ws* expr
   (* builtins *)
   <builtin>    = print
   print        = <'print'> ws* expr
   (* arithmethic *)
   <expr>       = add-sub | value | var | comparison
   <add-sub>    = mul-div | add | sub
   add          = add-sub ws* <'+'> ws* mul-div
   sub          = add-sub ws* <'-'> ws* mul-div
   <mul-div>    = term | mul | div
   mul          = mul-div ws* <'*'> ws* term
   div          = mul-div ws* <'/'> ws* term
   <term>       = float | int | var | <'('> ws* add-sub ws* <')'>
   <comparison> = lt
   lt           = expr ws* <'<'> ws* expr
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
  [[op & kids :as node]]
  (match node
    ;; builtins
    [:print x]
    (conj (compile x) [:print 1])
    ;; constants
    [:int i]
    [[:push (js/parseInt i)]]
    ;; arithmetic and comparison
    [(:or :add :sub :div :mul :lt) x y]
    (concatv (compile x) (compile y) [[op]])
    ;; variables
    [:assign [:var var-name] expr]
    (conj (compile expr) [:store var-name])
    [:var var-name]
    [[:load var-name]]
    ;; conditionals
    [:if test then else]
    (let [[test then else] (map compile [test then else])]
      (concatv test
               [[:ifjmp (+ 2 (count else))]]
               else
               [[:jmp (inc (count then))]]
               then))
    :else (throw (ex-info "Don't know how to compile" {:node node}))))

(defn doit
  []
  (println (compile (statement (parse "10 if x < 2 then x = x - 1 else print 2"))))
  ;; [[:load x]
  ;;  [:push 2]
  ;;  [:lt]
  ;;  [:ifjmp 4]
  ;;  [:push 2]
  ;;  [:print 1]
  ;;  [:jmp 5]
  ;;  [:load x]
  ;;  [:push 1]
  ;;  [:sub]
  ;;  [:store x]]
  )
