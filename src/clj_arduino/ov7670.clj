(ns clj-arduino.ov7670
  (:use :reload-all clj-arduino.core)
  (:use clodiuno.core)
  (:use clodiuno.firmata)
  (:require [clojure.pprint :refer [cl-format]]
            [clojure.core.async
             :as a
             :refer [>! <! >!! <!! go chan alts!! close! timeout pipeline]]))

;; http://embeddedprogrammer.blogspot.com/2012/07/hacking-ov7670-camera-module-sccb-cheat.html

(def i2c-address 0x21)
(def OV7670-COMMAND  0x40) ;; just testing


(def parameters (atom {:debug #{:printer}}))
;;(def img-size {:w 88 :h 144}) qcif
;;(def img-size {:w 44 :h 72}) ;; qqcif
;;(def img-size {:w 3 :h 37}) ;; qqcif, observed on Saleae
(def img-size {:w (/ 304 2) :h 60}) ;; qqvga,
(def accum (atom {}))
(def board)

(def ^:private readings> (chan (a/sliding-buffer 1024)))
(def ^:private out> (chan (a/sliding-buffer 1)))

(defn- write-bytes [conn & bs]
  (let [out (.getOutputStream (:port @conn))]
    (doseq [b bs]
      (.write out b))
    (.flush out)))


(defn decode-as-case
  [msg]
  (case (-> msg first short)
    1 "confirm case statement"
    2 "pinc & d, 4bit per byte"
    3 "set pixel clock and vsync timer delays"
    4 "88 bytes of vs, pckl flags"
    5 "vs time in microseconds"
    6 "pckl time in microseconds"
    7 "captured pixel counts"
    8 "data"
    9 "reset clock at 1mhz"
    10 "restore clock to 8mhz"
    11 "testing firmata string data"
    nil))

(def cmds {:test          0x01
           :some-pinc-d   0x02
           :timer-delay   0x03
           :some-timing   0x04
           :vsync-timing  0x05
           :pckl-timing   0x06
           :count-pixels  0x07
           :capture-image 0x08
           :clock-at-1mhz 0x09
           :clock-at-8mhz 0x0A
           :send-bytes    0x0B})

(defn cmd
  "send instruction to the firmata controller"
  [command & more]

  (doto board
    (write-bytes START-SYSEX
                 OV7670-COMMAND
                 (command cmds)))

  (when more (apply (partial write-bytes board) more))

  (write-bytes board END-SYSEX))


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

(defn fmt-timing-stream
  "reduce the vs & pckl stream
   vs and pckl vals alternate in the raw data stream"
  [bytes]
  (->>
   (reduce (fn [res x]
             (if (empty? res) (list [1 x])
                 (let [[lc lb] (first res)]
                   (if (= lb x)
                     (conj (rest res) [(inc lc) x])
                     (conj res [1 x]))))) [] bytes)
   reverse
;;   (remove #(= % [1 1]))
;;   (mapv first)
   ))

(defn fmt-timings
  "format the two timing streams
   vs and pckl vals alternate in the raw data stream"
  [msg]
  (let [bytes (->> (partition 4 msg))]
    (->> (mapv (fn [[pclk _ pincd _]] [pclk pincd]) bytes)
         (remove #(= % [0 0])))))

;;(cmd :some-timing)

(defn as-pixels
  [msg]
  (let [pixels (mapv (fn [[lsb msb]] 
                       [(byte lsb) (byte msb) (- 127 (bit-or (byte lsb) (<<< (byte msb) 7)))])
                     (partition 2 msg))]
    ;; first word of YUV channel should be grayscale luminance
    ;; so, partition 2 drops every other byte, retaining the Y channel, dropping the C channel
    (->> (map (comp last first) (partition 2 pixels)))))

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

(defn format-micros
  "format microseconds as secs, ms, us"
  [micros]
  (let [secs (quot micros 1000000)
        ms (-> micros (rem 1000000) (quot 1000))
        us (-> micros (rem 1000000) (rem 1000))]
    (format "%ds %dms %dus  -- %d" secs ms us micros)))



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
        msg (hash-map :data data :flag flag)]
;;    (println "msg flag" flag "msg bytes" (count sysex-string))
    (cond
      (ascii? sysex-string) (assoc msg :last-msg (as-ascii-string sysex-string))
      (= 0x00 flag) (assoc msg :case   (decode-as-int data))
      (= 0x02 flag) (assoc msg :pincd (pincd-callback data))
      (= 0x13 flag) (assoc msg :pcdl1 (decode-as-int data))
      (= 0x23 flag) (assoc msg :pcdl2 (decode-as-int data))
      (= 0x33 flag) (assoc msg :vsdl1 (decode-as-int data))
      (= 0x43 flag) (assoc msg :vsdl2 (decode-as-int data))
      (= 0x04 flag) (assoc msg :timing (fmt-timings data))
      (= 0x14 flag) (assoc-in msg [:delay :pc] (decode-as-int data))
      (= 0x24 flag) (assoc-in msg [:delay :vs] (decode-as-int data))
      (= 0x05 flag) (assoc msg :vsync  (decode-as-int data))
      (= 0x15 flag) (assoc msg :vscnt  (decode-as-int data))
      (= 0x06 flag) (assoc msg :pckl   (decode-as-int data))
      (= 0x07 flag) (assoc-in msg [:pixel-cnts :valid] (decode-as-int data))
      (= 0x17 flag) (assoc-in msg [:pixel-cnts :invalid] (decode-as-int data))
      (= 0x08 flag) (assoc msg :pixels (as-pixels data))
      (= 0x18 flag) (assoc msg :begin true)
      (= 0x28 flag) (assoc msg :line   (decode-as-int data))
      (= 0x38 flag) (assoc msg :offset (decode-as-int data))
      (= 0x48 flag) (assoc msg :row    (decode-as-int data))
      (= 0x58 flag) (assoc msg :sndtm  (decode-as-int data))
      (= 0x68 flag) (assoc msg :end    (decode-as-int data))
      (= 0x0B flag) (assoc msg :caseb  (count data))
      :else msg)))

(defn printer
  "print some elements of the reading payload"
  [{:keys [flag case vsync pckl begin end last-msg pincd timing delay
           pcdl1 pcdl2 vsdl1 vsdl2 line offset pixels vscnt row sndtm
           caseb] :as msg}]
  (when (and (not-empty msg)
             (debug? :printer))
    (cond
      last-msg (println "response:" last-msg)
      case (println (format "case %d: %s" case (decode-as-case [case])))
      vsync (println (format "vsync %s" (format-micros vsync)))
      vscnt (println "vsync count" vscnt)
      pckl (println (format "~pixel clock %s" (format-micros pckl)))
      timing (println timing)
;;      line  (println "> line" line " ")
;;      offset (println "> off" offset " ")
;;      pixels (println pixels)
      pcdl1 (println "pckl delay ms" pcdl1)
      pcdl2 (println "pckl delay us" pcdl2)
      vsdl1 (println "vs   delay ms" vsdl1)
      vsdl2 (println "vs   delay us" vsdl2)
      delay (println "delay:" delay)
      pincd (println pincd)
      begin (println "begin image capture")
      row   (println "line:" (:line @accum) (format-micros row))
      sndtm (println "firmata send overhead:" (format-micros sndtm))
      end   (println "end:" (format-micros end))
      caseb (println "bytes received" caseb)
      (= flag 0x17) (println "valid/invalid pixel cnts:"
                             (get-in @accum [:pixel-cnts :valid])
                             (get-in msg [:pixel-cnts :invalid]))))
  msg)

(defn accumulator
  "compile latest results"
  [{:keys [last-msg case pixel-cnts line offset data pixels begin] :as msg}]
  (cond
    begin (swap! accum assoc :image (make-array Byte/TYPE (* (:h img-size) (:w img-size))))
    pixel-cnts (swap! accum update-in [:pixel-cnts] merge pixel-cnts)
    pixels (let [image (:image @accum) ;; last image
                 pixels (vec pixels) 
                 line (:line @accum)
                 offset (:offset @accum)
                 pos (-> (dec line) (* (:w img-size)) (+ (* offset 49)))
                 num (count pixels)
;;                 _ (println "line" line "offset" offset "pos: " pos "num" num)
                 ]
             ;;             (swap! accum update :image #(doto % (aset pos pixels)))
             (swap! accum assoc :image 
                    (amap ^bytes image 
                          idx 
                          ret
                          (cond
                            (>= idx (+ pos num)) (byte 0)
                            (< idx pos) (byte (aget image idx))
                            :else (byte (get pixels (- idx pos)))))))
    (not-empty last-msg) (swap! accum merge msg)
    :else (swap! accum merge (dissoc msg :last-msg)) )
  msg)

(def xform (comp
            (map ov7670-decoder)
            (map accumulator)
            (map printer)))

(defonce p (pipeline 1 out> xform readings>))

;; default baudrate is 57600
;;  (def board (arduino :firmata (arduino-port)))
;;(def board (arduino :firmata (arduino-port) :baudrate 115200 :msg-callback msg-callback))

(defn- firmware
  [board]
  (when-let [{:keys [version name]} (:firmware @board)]
    [(as-ascii-string name) version]))

(defn open-board
  "open serial port to arduino"
  []
  (-> (alter-var-root 
       (var board) 
       (constantly
        (arduino :firmata (arduino-port) :baudrate 115200 :msg-callback msg-handler)))
      firmware))


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

(defn set-image-scaling
  "Image Scaling
  
  From the OV7670 implementation guide...

  Resolution Formats

  The Camera Chip has a set of pre-defined Scaler settings for QVGA,
  CIF and QCIF resolutinos.  These are set using COM7 (0x12).  To
  manually set the resolution, use registers COM14[3] (0x3e) and
  SCALING_PCLK_DELAY[7] (0xA2).

  After setting the desired resolution, adjust the window settings
  HSTART(0x17), HSTOP (0x18), HREF[5:0] (0x32), VSTRT (0x19),
  VSTOP (0x1A) and VREF[3:0] (0x03). To adjust these settings, set
  register TSLB[0] (0x3A) to 0.


  And...

  The Image Scaling circuit is composed of two blocks, Down Sampling
  and Digital Zoom Out.  Down Sampling supports 1/2^n scaling ratio
  and Digital Zoom Out performs fractional scaling.  Due to the buffer
  size, the maximum size that Digital Zoom Out can support is CIF.
  Combining the scaling ratio of down sampling and digital zoom out,
  the OV7670 CameraChip can support VGA, CIF and almost any size below
  CIF.

  For example, to get a 256x128 image, Down Sampling down samples VGA
  input to 320x240 by 1/2x ratio in both horizontal and vertical
  directions.  Then, the Digital Zoom Out scales the 320x240 input to
  125x128 by a scaling ratio of 0.8 horizontally and 0.53 vertically.

  Image Scaling Control Related Registers

  0x0C Common Control 3
       [3] enable digital zoom
       [2] enable down sampling
  0x73 Pixel Clock Divider
       [3] enable DSP output clock divider 
       [2:0] divider 1, 2, 4, 8 or 16 (yields 1,2,4 or 8 pixel clocks/byte)
  0x74 Horizontal Scaling Ratio
       [6:0] 0x20 or 0x40 for 1x or 0.5x scaling ratio
  0x75 Vertical Scaling Ratio
       [6:0] 0x20 or 0x40 for 1x or 0.5x scaling ratio
  0xA2 Pixel Clock Delay (offset to use when new scaled horizontal size is not a multiple of 640)
       [3:0] Original H size / [3:0] => New H size
    

  Down Sampling Control Related Registers

  0x72 Vertical and Horizontal down sampling
       [7] vertical average calculation option: truncation (0) or rounding (1)
       [6] vertical down sampling option: truncation (0) or rounding (1)
       [5:4] vertical down sampling rate: 0, 2, 4, or 8
       [3] horizontal average calculation option: truncation (0) or rounding (1)
       [6] horizontal down sampling option: truncation (0) or rounding (1)
       [1:0] horizontal down sampling rate: 0, 2, 4, or 8

  For example, qqvga  
	03E  {REG_COM14, 0x1a},	// divide by 4 ;; 0001 1010
        {0x72, 0x22},		// downsample by 4 ;; v & h down sampling
	{0x73, 0xf2},		// divide by 4 ;; hmm, looks like divide by 2"
  []
  ;; try dividing pixel clock by 16
  (let [com14 0x3e
        [reg _] (read-register com14)]
    (i2c-write board i2c-address com14 [(bit-or reg 2r1100)]))

  ;; try enabling digital zoom and down sampling
  (let [com03 0x0C
        [reg _] (read-register com03)]
    (i2c-write board i2c-address com03 [(bit-or reg 2r1100)]))

  ;; down sample v&h by 4
  (let [register 0x72
        [reg _] (read-register register)]
    (i2c-write board i2c-address register [(bit-or reg 2r100010)]))

  (let [register 0x73
        [reg _] (read-register register)]
    (i2c-write board i2c-address register [(bit-or reg 2r1010)])))

(defn set-register-bits
  "Update the value stored in a given register, setting bits in the bit-mask"
  [reg-addr bit-mask]
  (let [[old-val _] (read-register reg-addr)
        new-val (bit-or old-val bit-mask)]
    (i2c-write board i2c-address reg-addr [new-val])
    [old-val new-val]))

(defn qqcif!
  "
  Settings for QQCIF YUV mode  

  REG  VAL  Binary    Reg    Meaning
  ---- ---- --------- -----  ------------------------------------
  0x11 0x01 0000 0001 CLKRC  No prescaling of the internal clock 
  0x12 0x00 0000 0000 COM7   YUV format
  0x0C 0x0C 0000 1100 COM3   Enable zoom & down sampling
  0x3E 0x12 0001 0010 COM14  Enable DCW & PCLK scaling, divide PCLK by 4 
  0x70 0x3A 0011 1010        Horizontal scaling factor 58
  0x71 0x35 0011 0101        Vertical scaling factor 53
  0x72 0x22 0010 0010        Down sample H&V by 4
  0x73 0xF2 xxx1 0010        Pixel Clock Divider, divide by 4
  0xA2 0x2A 0010 1010        Pixel Clock Delay 42
"
  []
  (sorted-map :x11 (set-register-bits 0x11 0x01)
              :x12 (set-register-bits 0x12 0x00)
              :x0C (set-register-bits 0x0C 0x0C)
              :x3E (set-register-bits 0x3E 0x12)
              :x70 (set-register-bits 0x70 0x3A)
              :x71 (set-register-bits 0x71 0x35)
              :x72 (set-register-bits 0x72 0x22)
              :x73 (set-register-bits 0x73 0xF2)            
              :xA2 (set-register-bits 0xA2 0x2A)))

(defn qqvga!
  "
  Settings for QQVGA YUV mode  

  REG  VAL  Binary    Reg    Meaning
  ---- ---- --------- -----  ------------------------------------
  0x11 0x01 0000 0001 CLKRC  No prescaling of the internal clock 
  0x12 0x00 0000 0000 COM7   YUV format
  0x0C 0x04 0000 0100 COM3   Enable down sampling
  0x3E 0x1A 0001 1010 COM14  Enable manual scaling, divide PCLK by 4
  0x70 0x3A 0011 1010        Horizontal scaling factor 58
  0x71 0x35 0011 0101        Vertical scaling factor 53
  0x72 0x22 0010 0010        Down sample H&V by 4
  0x73 0xF2 xxx1 0010        Pixel Clock Divider, divide by 4
  0xA2 0x02 0000 0010        Pixel Clock Delay 2
"
  []
  (sorted-map :x11 (set-register-bits 0x11 0x01)
              :x12 (set-register-bits 0x12 0x00)
              :x0C (set-register-bits 0x0C 0x04)
              :x3E (set-register-bits 0x3E 0x1A)
              :x70 (set-register-bits 0x70 0x3A)
              :x71 (set-register-bits 0x71 0x35)
              :x72 (set-register-bits 0x72 0x22)
              :x73 (set-register-bits 0x73 0xF2)            
              :xA2 (set-register-bits 0xA2 0x02)))

(defn initialize
  []
  (i2c-init board)
  (qqvga!)
  (pckl-off-when-hblanking)
  (cmd :clock-at-1mhz)
  "ok")

(comment
  @accum
  (get-in @accum [:image :l144])
  (open-board)
  (close board)
  (initialize)
  (i2c-init board)
  (qqvga!)
  (qqcif!)
  (pckl-off-when-hblanking)
  (soft-reset!)
  (set-image-scaling)
  (cmd :capture-image)
  (cmd :send-bytes)
  (cmd :test)
  (cmd :clock-at-1mhz)
  (cmd :clock-at-8mhz)
  (cmd :vsync-timing)
  ;; [msb, lsb] pc ms delay; pc us; vs ms; vs us 
  (doseq [n (range 0 100)]
    (cmd :timer-delay 0 27 0 n 0 0 0 0)
    (Thread/sleep 500)
    (cmd :some-timing))
  (cmd :some-pinc-d)
  (cmd :count-pixels)
  (cmd :pckl-timing)
  (cmd :test))


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

(comment
  ;; initialize I2C before doing anything else
  (i2c-init board)

  (i2c-blocking-read board i2c-address 0x01 100 :timeout 1000)
  
  (i2c-blocking-read board i2c-address com7 1 :timeout 1000)



  )
