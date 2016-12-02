(ns lbbasic.messages)

(defn send!
  ([worker type js-object]
   (set! (.-type js-object) (name type))
   (.postMessage worker js-object))
  ([type js-object]
   (set! (.-type js-object) (name type))
   (js/postMessage js-object)))
