(ns lbbasic.ui.components
  (:require [hoplon.core :as h :refer-macros [defelem]]))

(defn removeChildNative
  [obj child]
  (.call @#'h/removeChild obj child))

(defn appendChildNative
  [obj child]
  (.call @#'h/appendChild obj child))

(defelem make-line-printer
  "Returns a map with two keys.
  :println! - a function of one argument, the string to print in the textarea
  :textarea - an HTML text area that displays the strings appended so far.

  The max-rows argument governs how many rows may appear in the text area (the
  scrollback buffer) before dropping lines."
  [{:keys [max-rows] :or {max-rows 10} :as attrs} _]
  (let [area (h/textarea (dissoc attrs :max-rows))]
    {:println! (fn [line]
                 (let [n (.. area -childNodes -length)]
                   (if (= n 0)
                     (appendChildNative area (.createTextNode js/document (str line)))
                     (let [last-line (aget (.-childNodes area) (dec n))]
                       (set! (.-textContent last-line) (str (.-textContent last-line) \newline))
                       (appendChildNative area (.createTextNode js/document line))
                       (when (= n max-rows) (removeChildNative area (aget (.-childNodes area) 0))))))
                 (set! (.-scrollTop area) (.-scrollHeight area)))
     :clear! (fn [] (.empty (js/jQuery area)))
     :textarea area}))
