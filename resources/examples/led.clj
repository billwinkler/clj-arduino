(ns led
  (:use :reload-all clj-arduino.core)
  (:use :reload-all clodiuno.core)
  (:use :reload-all clodiuno.firmata))

(def the-port (-> (find-arduino-ports) keys last))

(def board (arduino :firmata the-port))

(pin-mode board 13 OUTPUT)

(doseq [_ (range 5)] 
  (digital-write board 13 HIGH)
  (Thread/sleep 1000)
  (digital-write board 13 LOW)
  (Thread/sleep 1000))

(close board)
