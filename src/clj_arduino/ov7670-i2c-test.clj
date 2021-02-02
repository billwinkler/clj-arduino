(ns clj-arduino.ov7670-i2c-test
  (:use :reload-all clj-arduino.core)
  (:use :reload-all clodiuno.core)
  (:use :reload-all clodiuno.firmata)
  (:require [clojure.pprint :refer [cl-format]]))

;; load ov7670_test_2 to the Arduino
(def i2c-address 0x21)

;; default baudrate is 57600
(def board (arduino :firmata (arduino-port)))

(defn- ->bits [i]
  "zero padded bit string"
  (cl-format nil " ~2,'0x ~8,'0b" i i))

(defn read-registers
  "get the OV7670 register values as a map"
  []
  (->> (i2c-blocking-read board i2c-address 0x00 200 :timeout 500)
       (map-indexed (fn [idx itm]
                      (hash-map
                       (->> (Integer/toHexString idx)
                            (clojure.string/upper-case)
                            (str "r") keyword) itm)))
       (reduce into)))

(type *registers*)

(def ^:dynamic *registers* (read-registers))

(defn pixel-format
  ""
  []
  (condp = (-> (:r12 *registers*) (bit-and 2r00000101))
    2r00000000 :yuv
    2r00000100 :rgb
    2r00000001 :raw-bayer
    2r00000100 :processed-bayer
    ))



(comment
  ;; initialize I2C before doing anything else
  (i2c-init board)

  (i2c-blocking-read board i2c-address 0x01 1 :timeout 1000)
  
  (i2c-blocking-read board i2c-address com7 1 :timeout 1000)


  (close board)

  )
