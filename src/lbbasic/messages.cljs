(ns lbbasic.messages)

(defn send!
  "For sending a message to or from a Worker. With a worker argument, sends a
  message to that worker. Without a worker argument, sends a message to the
  parent process."
  ([worker type js-object]
   (set! (.-type js-object) (name type))
   (.postMessage worker js-object))
  ([type js-object]
   (set! (.-type js-object) (name type))
   (js/postMessage js-object)))
