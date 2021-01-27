(ns scale
  (:use clj-arduino.hx711)
  (:require [clojure.core.async :refer [go]]))

;; HX711 DT connected to pin 2
;; HX711 SCK connected to pin 3

(defn pecan-handler
  "do something with incoming weight measurement events"
  [{:keys [raw weight]}]
  (let [assessment (when weight
                     (condp > weight
                       0.2 :unloaded
                       6 :no-bueno
                       8 :bueno
                       :si-bueno))]
    
    (when weight (println weight assessment))))

(defn process!
  "process events"
  []
  (go (while (running?) (-> (get-next) (pecan-handler)))))

(comment
  (start!)
  (tare!)
  (calibrate!)
  (process!)
  (get-next)
  (stop!)
  (debug!)
  (debug! :printer)
  )



