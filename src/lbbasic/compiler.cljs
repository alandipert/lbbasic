(ns lbbasic.compiler
  (:require
   [adzerk.cljs-console :as log :include-macros true]
   [cljs.core.match :refer-macros [match]]
   [cljs.pprint :as pp]
   [clojure.string :as str]
   [instaparse.core :as insta]
   [lbbasic.util :refer [concatv]]))

(def parser
  (insta/parser
   ;;{{
   line           = (linum ws)? stmts
   linum          = #'(0|([1-9][0-9]*))'
   <stmts>        = stmt (ws <':'> ws stmt)*
   <stmt>         = assign | if | builtin | goto
   (* if *)
   then           = stmts
   else           = stmts
   if             = <'if'> ws expr ws <'then'> ws then ws (<'else'> ws else ws)? <'fi'>
   (* assignment *)
   assign         = var ws <'='> ws expr
   (* goto *)
   goto           = <'goto'> ws linum
   (* builtins *)
   <builtin>      = print-newline | print-adjacent | print-tab
   print-newline  = <'print'>
   print-adjacent = <'print'> ws expr (ws <';'> ws expr)*
   print-tab      = <'print'> ws expr (ws <','> ws expr)+
   (* arithmethic *)
   <expr>         = add-sub | value | var | comparison
   <add-sub>      = mul-div | add | sub
   add            = add-sub ws <'+'> ws mul-div
   sub            = add-sub ws <'-'> ws mul-div
   <mul-div>      = term | mul | div
   mul            = mul-div ws <'*'> ws term
   div            = mul-div ws <'/'> ws term
   <term>         = float | int | var | <'('> ws add-sub ws <')'>
   <comparison>   = eq | lt | lte | gt | gte
   eq             = expr ws <'=='> ws expr
   lt             = expr ws <'<'> ws expr
   lte            = expr ws <'<='> ws expr
   gt             = expr ws <'>'> ws expr
   gte            = expr ws <'>='> ws expr
   (* literals *)
   <value>        = string | float | int
   string         = #'\"[^\"]+\"'
   float          = #'[+-]?(0|([1-9][0-9]*))(\.[0-9]+)'
   int            = #'[+-]?(0|([1-9][0-9]*))'
   (* variable reference *)
   var            = #'[a-zA-Z][a-zA-Z0-9]*'
   (* util *)
   <ws>           = <#'\s*'>
   ;;}}
   ))

(defn linum
  [parsed]
  (match parsed
    [:line [:linum n] & _] (.parseInt js/window n)
    :else nil))

(defn statements
  [parsed]
  (match parsed
    [:line [:linum _] & stmts] stmts
    [:line & stmts] stmts
    :else (throw (ex-info "Unknown parse result" {:parsed parsed}))))

(defn pr-asm
  [[x & xs :as asm]]
  (if (= 1 (count asm))
    (pr-str asm)
    (str "[" (pr-str x) "\n"
         (str/join "\n" (map #(str " " (pr-str %)) xs))
         "]")))

(def print-sep
  {:print-adjacent ""
   :print-tab \tab})

(defn compile1
  [[op :as node]]
  (match node
    ;; builtins
    [(:or :print-adjacent :print-tab) & exprs]
    (concatv (mapcat compile1 exprs)
             [[:print (count exprs) (print-sep op)]])
    [:print-newline]
    [[:print 0 ""]]
    ;; constants
    [:int i]
    [[:push (js/parseInt i)]]
    [:string s]
    [[:push (.parse js/JSON s)]]
    [:float i]
    [[:push (js/parseFloat i)]]
    ;; operators
    [(:or :eq :add :sub :div :mul :lt :lte :gt :gte) x y]
    (concatv (compile1 x) (compile1 y) [[op]])
    ;; variable assignment and reference
    [:assign [:var var-name] expr]
    (conj (compile1 expr) [:store var-name])
    [:var var-name]
    [[:load var-name]]
    ;; conditionals
    [:if test [:then & then]]
    (let [test (compile1 test)
          then (mapcat compile1 then)]
      (concatv test
               [[:ifjmp 2]]
               [[:jmp (inc (count then))]]
               then))
    [:if test [:then & then] [:else & else]]
    (let [test (compile1 test)
          then (mapcat compile1 then)
          else (mapcat compile1 else)]
      (concatv test
               [[:ifjmp (+ 2 (count else))]]
               else
               [[:jmp (inc (count then))]]
               then))
    ;; goto
    [:goto [:linum linum]]
    [[:goto (js/parseInt linum)]]
    :else (throw (ex-info "Don't know how to compile1" {:node node}))))

(defn compile
  [nodes]
  (vec (mapcat compile1 nodes)))

(defn compile-line
  [line]
  (let [parsed (insta/parse parser line)]
    (if (insta/failure? parsed)
      (throw (ex-info "Parse error" {:failure (insta/get-failure parsed)}))
      (let [stmts (statements parsed)]
        {:line  line
         :linum (linum parsed)
         :stmts stmts
         :insts (compile stmts)}))))

(defn doit
  []
  (time
   (-> ;; "10ifx>1{print1:print2::printx}"
       ;; "10?x>1{x=x-1}:printx"
       "10 if x1 > 1 then print x1 else print x fi"
       ;;"10 print x"
       parser
       ;; statements
       ;; compile
       println
       ))
  ;; (let [prog1 "10 print \"got here\",1,2,3 :if x==2 then x=x-1 else print 2.3 fi: print \"fin\";x < 2"
  ;;       prog2 "10 if x <= 123 then print 321,3 fi:Y = Y *3:goto 123"
  ;;       ;; prog2 "10 if x <= 3 then print 321,3: Y = Y*3"
  ;;       prog3 "10 if x * 2 < 5 then print 1,2,3"
  ;;       ;; prog3 "10 print 123: x=x+1:print x"
  ;;       prog {10 "10 n = 99"
  ;;             15 "10 print"
  ;;             20 "20 print n;\" bottles of beer on the wall, \";n;\" bottles of beer\""
  ;;             30 "30 print \"take one down and pass it around, \";n-1;\" bottles of beer on the wall.\""
  ;;             40 "40 if n > 1 then n=n-1:goto 20 fi"}]
  ;;   #_(println (pr-asm (compile (statements (parse prog2)))))
  ;;   (doseq [[line s] prog]
  ;;     (println line)
  ;;     (println (statements (parse s)))
  ;;     (println (pr-asm (compile (statements (parse s)))))
  ;;     ))
  )

