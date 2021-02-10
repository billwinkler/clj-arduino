(ns clj-arduino.ov7670
  (:use :reload-all clj-arduino.core)
  (:use :reload-all clodiuno.core)
  (:use :reload-all clodiuno.firmata)
  (:require [clojure.pprint :refer [cl-format]]
            [clojure.core.async
             :as a
             :refer [>! <! >!! <!! go chan alts!! close! timeout pipeline]]))

;; http://embeddedprogrammer.blogspot.com/2012/07/hacking-ov7670-camera-module-sccb-cheat.html

(def i2c-address 0x21)
(def OV7670-COMMAND  0x40) ;; just testing


(def parameters (atom {:debug #{:printer}}))
(def img-size {:w 88 :h 144})
(def accum (atom {}))
(def board)

(def ^:private readings> (chan (a/sliding-buffer 10)))
(def ^:private out> (chan (a/sliding-buffer 1)))

(defn- write-bytes [conn & bs]
  (let [out (.getOutputStream (:port @conn))]
    (doseq [b bs]
      (.write out b))
    (.flush out)))

(defn- lsb [b]
  (bit-and b 0x7F))

(defn- msb [b]
  (bit-and (bit-shift-right b 7) 0x7F))

(defn- bytes-to-int [lsb msb]
  (bit-or (bit-shift-left (bit-and msb 0x7F) 7) 
          (bit-and lsb 0x7F)))

(defn- write-data [conn data]
  (when (not (empty? data))
    (apply write-bytes conn 
           (mapcat (fn [b] [(lsb b) (msb b)])
                   data))))

(defn- bits [n]
  (->> (map #(bit-and (bit-shift-right n %) 1) (range 8))
      reverse
      (apply str)))

 (defn as-hex-array
   [msg]
   (mapv #(format "%02X" (byte %)) msg))

 (defn as-pixels
   [msg]
   ;; expected number of pixels per row is 88 for qcif grayscale channel
   (let [pixels (mapv (fn [[lsb msb]] 
                        [(byte lsb) (byte msb) (bit-or (byte lsb) (<<< (byte msb) 7))])
                      (partition 2 msg))]
     ;; first word of YUV channel should be grayscale luminance
     (->> (map (comp last first) (partition 2 pixels))
          (into-array Byte/TYPE))))

 (defn ascii?
   [msg]
   (->> (partition 2 msg) (map (comp long first)) (every? #(< 0x19 % 0x7f))))

 (defn as-ascii-string
   [msg]
   (->> (partition 2 msg) (map first) (apply str)))

 (defn decode-as-int
   [msg]
   (let [[b0 b1 b2 b3] (->> (map byte msg) (partition 2) (map first))]
     (+ b0 (<<< b1 7) (<<< b2 14) (<<< b3 21))))

 (defn decode-as-case
   [msg]
   (case (-> msg first byte)
     1 "confirm case statement"
     3 "pinc & d, 4bit per byte"
     4 "40 bytes of vs, pckl flags"
     5 "vs time in microseconds"
     6 "pckl time in microseconds"
     7 "captured pixel counts"
     8 "data"
     9 "reset clock at 1mhz"
     10 "restore clock to 8mhz"
     nil))

 (defn pincd-callback
   [msg]
   (mapv (fn [[m0 m1 m2 m3]]
           [(bits (bit-or (bit-and (<<< m0 4) 0xf0) (bit-and m1 0x0f)))
            (bits (bit-or (bit-and (<<< m2 4) 0xf0) (bit-and m3 0x0f)))])
         (->> (map byte msg) (partition 4))))

(defn debug?
  "test debug settings"
  [some-key]
  (contains? (:debug @parameters) some-key))

(defn debug!
  "Clear debug flags by default, or set them to some elements"
  [& flags] (:debug (swap! parameters assoc-in [:debug] (set flags))))

(defn msg-handler
  "generate a callback function; set state machine to :init "
  [msg]
  (a/put! readings> msg))

(defn ov7670-decoder
  "decode the sysex responses"
  [sysex-string]
  (let [raw (map long sysex-string)
        flag (first raw)
        data (drop 2 raw)
        msg (hash-map :data data)]
    (cond
      (= 0x00 flag) (assoc msg :case   (decode-as-int data))
      (= 0x03 flag) (assoc msg :pinc-d (pincd-callback data))
      (= 0x04 flag) (assoc msg :timing (as-hex-array data))
      (= 0x05 flag) (assoc msg :vsync  (decode-as-int data))
      (= 0x06 flag) (assoc msg :pckl   (decode-as-int data))
      (= 0x07 flag) (assoc-in msg [:pixel-cnts :valid] (decode-as-int data))
      (= 0x17 flag) (assoc-in msg [:pixel-cnts :invalid] (decode-as-int data))
      (= 0x08 flag) (assoc msg :pixels (as-pixels data))
      (= 0x18 flag) (assoc msg :begin true)
      (= 0x28 flag) (assoc msg :line (decode-as-int data))
      (= 0x38 flag) (assoc msg :end  (decode-as-int data))
      (ascii? data) (assoc msg :last-msg (as-ascii-string msg)))))

(defn printer
  "print some elements of the reading payload"
  [{:keys [case vsync begin end last-msg] :as msg}]
  (when (and (not-empty msg)
             (debug? :printer))
    (cond
      last-msg (println "last-msg:" last-msg)
      case (println "case:" case)
      begin (println "begin:" begin)
      end (println "end:" end)))
  msg)


(defn accumulator
  "compile latest results"
  [{:keys [last-msg case pixel-cnts line data pixels begin] :as msg}]
  (cond
    begin (swap! accum assoc :image (make-array Byte/TYPE (:h img-size) (:w img-size)))
    pixel-cnts (swap! accum update-in [:pixel-cnts] merge pixel-cnts)
    pixels (swap! accum update :image #(doto % (aset (-> @accum :line dec) pixels)))
    (not-empty last-msg) (swap! accum merge msg)
    :else (swap! accum merge (dissoc msg :last-msg)) )
  msg)

(def xform (comp
            (map ov7670-decoder)
            (map accumulator)
            (map printer)))

(defonce p (pipeline 1 out> xform readings>))

 (defn msg-callback
   [msg]
   (let [flag (-> msg first long)
         data (drop 2 msg)]
     (when-let [case (decode-as-case msg)] (println case))
     (cond
       (= 0x00 flag) (println "case: " (decode-as-int data))
       (= 0x03 flag) (println "pinc&d: " (pincd-callback data))
       (= 0x04 flag) (println "vsync, pckl: " (as-hex-array data))
       (= 0x05 flag) (println "vs: " (decode-as-int data))
       (= 0x06 flag) (println "pckl: " (decode-as-int data))
       (= 0x07 flag) (println "pixels: " (decode-as-int data))
       (= 0x17 flag) (println "invalid: " (decode-as-int data))
       (= 0x08 flag) (println (take 60 (as-pixels data)))
       (= 0x18 flag) (println "begin frame")
       (= 0x28 flag) (println "line: " (decode-as-int data))
       (= 0x38 flag) (println "time: " (decode-as-int data))
       (= 8 (count data)) (println "int-> " (decode-as-int data))
       (ascii? data) (println (as-ascii-string msg))
       ;; print first n
       :else (println (take 20 (as-hex-array msg))))))

;; default baudrate is 57600
;;  (def board (arduino :firmata (arduino-port)))
;;(def board (arduino :firmata (arduino-port) :baudrate 115200 :msg-callback msg-callback))
(def board (arduino :firmata (arduino-port) :baudrate 115200 :msg-callback msg-handler))

;;(close board)
(def cmds {:test          0x01
           :some-pinc-d   0x03
           :some-timing   0x04
           :vsync-timing  0x05
           :pckl-timing   0x06
           :count-pixels   0x07
           :capture-image 0x08
           :clock-at-1mhz 0x09
           :clock-at-8mhz 0x0A})

(defn cmd
  "send instruction to the firmata controller"
  [cmd]
  (doto board
    (write-bytes START-SYSEX
                 OV7670-COMMAND
                 (cmd cmds)
                 END-SYSEX)))

(comment
  @accum
  (get-in @accum [:image :l144])
  (initialize)
  (close board)
  (cmd :capture-image)
  (cmd :some-pinc-d)
  (cmd :clock-at-1mhz)
  (cmd :clock-at-8mhz)
  (cmd :vsync-timing)
  (cmd :count-pixels)
  (cmd :pckl-timing)
  (cmd :test)

)

(comment 
  (i2c-init board)
  (close board)
  (read-register 1)
  ;; video format
  (read-register 0x12)
  (enable-scaling)
  (set-video-format :qcif)

  ;; bit 5 PCLK does not toggle on horizontal blank
  (read-register 0x15)
  (i2c-write board i2c-address 0x15 [(bit-set 0 5)])

  (debug! :printer)
  (debug!)

)


(defn- ->bits [i]
  "zero padded bit string"
  (cl-format nil " ~2,'0x ~8,'0b" i i))

(defn read-register
  "get the OV7670 register values as a map"
  [addr]
  (i2c-blocking-read board i2c-address addr 1 :timeout 500))

(defn enable-scaling
  "scaling needs to be enabled for QCIF (176x144) format"
  []
  (let [addr 0x0C
        [reg _] (read-register addr)]
    (i2c-write board i2c-address addr [(bit-or reg 2r1000)])))

(defn pckl-off-when-hblanking
  "suppresses the pixel clock with not capturing data bits"
  []
  (i2c-write board i2c-address 0x15 [(bit-set 0 5)]))

(defn soft-reset!
  "reset registers to default values"
  []
  (i2c-write board i2c-address 0x12 [0x80]))

;;(read-register 0x0c)

(defn set-video-format
  "one of :cif, :qcif, :qvga"
  [fmt]
  (let [addr 0x12
        [reg _] (read-register addr)
        _ (println "reg" (->bits reg))
        bits (-> reg (bit-and 2r11000111)
                 (bit-or 
                  (case fmt
                    :cif  2r00100000
                    :qvga 2r00010000
                    :qcif 2r00001000)))
        _ (println "bits" (->bits bits))]
    (i2c-write board i2c-address addr [bits])))

;;(set-video-format :qcif)

(defn pixel-format
  ""
  []
  (condp = (-> (read-register 0x12) (bit-and 2r00000101))
    2r00000000 :yuv
    2r00000100 :rgb
    2r00000001 :raw-bayer
    2r00000100 :processed-bayer
    ))

(defn initialize
  []
  (i2c-init board)
  (enable-scaling)
  (set-video-format :qcif)
  (pckl-off-when-hblanking)
  "ok")


(comment
  ;; initialize I2C before doing anything else
  (i2c-init board)

  (i2c-blocking-read board i2c-address 0x01 100 :timeout 1000)
  
  (i2c-blocking-read board i2c-address com7 1 :timeout 1000)



  )
