(ns lbbasic.pp
  (:require [clojure.string :as str]))

(defn pr-asm
  [[x & xs :as asm]]
  (if (= 1 (count asm))
    (pr-str asm)
    (str "[" (pr-str x) "\n"
         (str/join "\n" (map #(str " " (pr-str %)) xs))
         "]")))
