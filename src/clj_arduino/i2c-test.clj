(ns clj-arduino.i2c-test
  (:use :reload-all clj-arduino.core)
  (:use :reload-all clodiuno.core)
  (:use :reload-all clodiuno.firmata))

;; load the StandardFirmata example sketch to the Master Arduino
;; load i2c_slave_2 to the Slave Arduino

;; Connect I2C bus pins 4 & 5 from Master to Slave
;; +5V & GND from Master to Slave
;; Connect LED negative pins to ground
;; Slave pin 7 to red LED + 
;; Slave pin 6 to green LED +
;; Slave pin 5 to yello LED +


;; default baudrate is 57600
(def board (arduino :firmata (arduino-port)))

(comment
  ;; initialize I2C before doing anything else
  (i2c-init board)

  
  (i2c-write board 4 [0x05]) ;; turn on yellow LED
  (i2c-write board 4 [0x06]) ;; green 
  (i2c-write board 4 [0x07]) ;; any other number should flash red

  (close board)

)
