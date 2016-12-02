(ns lbbasic.vm-worker
  (:require [lbbasic.vm :as vm]
            [lbbasic.messages :as msg]
            [clojure.data.avl :as avl]
            [javelin.core :refer [cell] :refer-macros [cell=]]))

(enable-console-print!)

(def default-interrupts
  {:break []})

(def interrupts (cell default-interrupts))

(def break?
  (cell= (not (empty? (:break interrupts)))))

(defn clear!
  [interrupt-name]
  (swap! interrupts update interrupt-name empty))

(defmulti cmd #(.-type %))

(defmethod cmd "interrupt"
  [msg]
  (let [name  (keyword (.-name msg))
        value (.-value msg)]
    (swap! interrupts update name conj value)))

(defmethod cmd "run"
  [msg]
  (reset! interrupts default-interrupts)
  (let [init (merge (vm/deserialize-machine (.-machine msg))
                    {:line     (.-line msg)
                     :inst-ptr 0
                     :printfn  #(msg/send! :print (js-obj "line" %))})]
    (letfn [(tramp [prev]
              (let [next (vm/step prev 10)]
                (if (= next prev)
                  (msg/send! :halt (js-obj "machine" (vm/serialize-machine next)))
                  (if @break?
                    (do (clear! :break)
                        (msg/send! :break (js-obj "machine" (vm/serialize-machine next))))
                    (js/setTimeout #(tramp next) 0)))))]
      (js/setTimeout #(tramp init) 0))))

(defn init []
  (set! js/onmessage (fn [msg] (-> msg .-data cmd))))

