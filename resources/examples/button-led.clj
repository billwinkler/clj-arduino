(ns button-led
  (:use :reload-all clodiuno.core))

;; Button connected to pin 6
;; LED connected ro pin 3
;; http://www.arduino.cc/en/Tutorial/Button

(def board (arduino :firmata))

(pin-mode board 3 OUTPUT)
(pin-mode board 6 INPUT)

(while true 
       (let [i (digital-read board 6)] 
	 (println i)
	 (digital-write board 3 i)))

;;(close board)
