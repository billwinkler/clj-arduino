(ns clj-arduino.ov7670
  (:use :reload-all clj-arduino.core)
  (:use clodiuno.core)
  (:use clodiuno.firmata)
  (:require [clojure.pprint :refer [cl-format]]
            [clojure.core.async :as a :refer [chan pipeline]]))

;; http://embeddedprogrammer.blogspot.com/2012/07/hacking-ov7670-camera-module-sccb-cheat.html

(def i2c-address 0x21)
(def OV7670-COMMAND  0x40) ;; Extending SYSEX with 0x40 command

(def parameters (atom {:debug #{:printer}}))
;; 160x120x2 frame (QQVGA)
(def img-size {:w 130 :h 120}) ;; qqvga, YUV gray scale pixels
(def accum (atom {}))
(def board)

(def ^:private readings> (chan (a/sliding-buffer 20000)))
(def ^:private out> (chan (a/sliding-buffer 1)))

(def ^{:doc " 
OV7670 register coordinates. Entries are one of: 
  - a register address, 
  - an address slice,
  - or a vector of address slices"
       :private true}
  registers (-> "registers.edn" clojure.java.io/resource slurp read-string))



(defn- write-bytes [conn & bs]
  (let [out (.getOutputStream (:port @conn))]
    (doseq [b bs]
      (.write out b))
    (.flush out)))


(defn decode-as-case
  [msg]
  (case (-> msg first short)
    1 "confirm case statement"
    2 "initial pixel clock low timing"
    3 "set pixel clock and vsync timer delays"
    4 "100 data samples at given offset"
    5 "vs time in microseconds"
    6 "pckl time in microseconds"
    7 "captured pixel counts"
    8 "data"
    9 "reset clock at 1mhz"
    10 "restore clock to 8mhz"
    nil))

(def cmds {:test          0x01
           :first-pulse   0x02
           :timer-delay   0x03
           :some-timing   0x04
           :vsync-timing  0x05
           :pckl-timing   0x06
           :count-pixels  0x07
           :capture-image 0x08
           :clock-at-1mhz 0x09
           :clock-at-8mhz 0x0A})

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

(defn- ->bits [i]
  "zero padded bit string"
  (cl-format nil "~8,'0b" i))

(defn as-hex-array
  "Format the returned message values as hex values for display purposes."
  [msg]
  (mapv #(format "%02X" (byte %)) msg))

(defn fmt-timings
  "Format the `case 4` timing stream.  Display the distinct pixels
  captured during periods when the pixel clock is low.  The raw timing
  data stream is 4 bytes wide.  First byte is 1 when pixel clock is
  high and 0 when low.  Second byte is the MSB of the pixel clock
  which is always zero.  The next two bytes are the D7 to D0 bits."
  [msg]
  (let [bytez (->> (partition 4 msg))
        fmted  (->> (mapv (fn [[pclk _ pin-lsb pin-msb]]
                            [(case pclk 0 :l 1 :h)  (->bits (bit-or pin-lsb (<<< pin-msb 8)))]) bytez))
        samps (for [[clk data] fmted :when (= clk :l)] data)]
    ;; drop duplicates
    (reduce (fn [[last :as all] samp] (if (= samp last) all (conj all samp))) () samps)))

(defn as-pixels
  "Interpret the message data as a gray scale (Y channel) pixel stream."
  [msg]
  (mapv (fn [[lsb msb]] (- 127 (bit-or (byte lsb) (<<< (byte msb) 7))))
        (partition 2 msg)))

(defn ascii?
  "Returns true when all values in a message are printable characters"
   [msg]
   (->> (partition 2 msg) (map (comp long first)) (every? #(< 0x19 % 0x7f))))

(defn as-ascii-string
  "Format the message as an ASCII string"
   [msg]
   (->> (partition 2 msg) (map first) (apply str)))

(defn decode-as-int
  "Interpret the message as one sent as STRING_DATA, four characters in
  length, encoded via the `send_int` function.  Each character holds 7
  bits of the integer value."
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
    (cond
      (ascii? sysex-string) (assoc msg :last-msg (as-ascii-string sysex-string))
      (= 0x00 flag) (assoc msg :case   (decode-as-int data))
      (= 0x02 flag) (assoc msg :pulse  (decode-as-int data))
      (= 0x13 flag) (assoc msg :pcdl1  (decode-as-int data))
      (= 0x23 flag) (assoc msg :pcdl2  (decode-as-int data))
      (= 0x04 flag) (assoc msg :timing (fmt-timings data))
      (= 0x14 flag) (assoc msg :delay  (decode-as-int data))
      (= 0x24 flag) (assoc msg :period (decode-as-int data))
      (= 0x05 flag) (assoc msg :vsync  (decode-as-int data))
      (= 0x06 flag) (assoc msg :pckl   (decode-as-int data))
      (= 0x07 flag) (assoc msg :pcount (decode-as-int data))
      (= 0x08 flag) (assoc msg :pixels (as-pixels data))
      (= 0x18 flag) (assoc msg :begin true)
      (= 0x28 flag) (assoc msg :end    (decode-as-int data))
      :else msg)))

(defn printer
  "print some elements of the reading payload"
  [{:keys [flag case last-msg pulse pcdl1 pcdl2 timing delay 
           period vsync pckl pcount pixels begin end] :as msg}]
  (when (and (not-empty msg)
             (debug? :printer))
     (cond
      last-msg (println "response:" last-msg)
      case   (println (format "case %d: %s" case (decode-as-case [case])))
      vsync  (println (format "vsync %s" (format-micros vsync)))
      pckl   (println (format "~pixel clock %s" (format-micros pckl)))
      pulse  (println "first pulse" (format-micros pulse))
      pcdl1  (println "pckl delay ms" pcdl1)
      pcdl2  (println "pckl delay us" pcdl2)
      delay  (println "delay:" (format-micros delay))
      period (println "period:" (format-micros period))
      timing (println timing)
      pcount (println "pixel count:" pcount)
      begin  (println "begin image capture")
      pixels (println (count pixels) "pixels captured")
      end    (println "end:" (format-micros end))))
  msg)

(defn accumulator
  "compile latest results"
  [{:keys [last-msg case pixel-cnts line offset data pixels begin] :as msg}]
  (cond
    begin (swap! accum assoc :image (make-array Byte/TYPE (* (:w img-size) (:h img-size))))
    pixel-cnts (swap! accum update-in [:pixel-cnts] merge pixel-cnts)
    pixels (let [image (:image @accum) ;; last image
                 pixels (vec pixels)]
             (swap! accum assoc :image 
                    (amap ^bytes image 
                          idx 
                          ret
                          (cond
                            (>= idx (count pixels)) (byte 0) ;; pad with zeros
                            :else (byte (get pixels idx))))))
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

(defn read-register
  "get the OV7670 register value for register at a given address"
  [addr]
  (first (i2c-blocking-read board i2c-address addr 1 :timeout 500)))

(defn- register-bits
  "Get the value of a given range of bits within a given register
  address. Format the return for debug printing.  The output is in the
  form:

  [label val \"val-bin-str val-hex-str reg-bin-str[slice]\"

  label      :  register name (optional)
  val        :  decimal value of the slice
  val-bin-str:  val formatted as binary string
  val-hex-str:  val formatted as hex sting
  reg-bin-str:  register bits as binary string 
  slice      :  bit from/to slice range"
  ([addr] (register-bits addr [7 0]))
  ([addr [b1 b0] & label]
   (let [reg (read-register addr)
         ;; strip off the bits within the given slice range
         bitz (apply str 
                     (for [x (range 7 -1 -1)
                           :let [y (if (>= b1 x b0) 1 0)
                                 z (bit-and 1 (bit-shift-right reg x))]
                           :when (pos? y)] z))
         val (read-string (str "2r" bitz))
         sval (format "%s 0x%02X  %s[%d:%d]"  bitz val (->bits reg) b1 b0)]
     [val (if-not label sval (-> label first (str " " sval)))])))

(defn register
  "Get the register value for a given register name"
  [name]
  (let [[reg & [slice]] (registers name)]
    (cond 
      (nil? reg) nil
      (vector? reg) (-> (map (fn [[nm slice]] (register-bits (-> nm registers first) slice nm)) 
                             (registers name))
                        (into (list (str name))))
      (not-empty slice) (register-bits reg slice name)
      :else (register-bits reg [7 0] name))) )

(defn set-register-bits
  "Update the value stored in a given register, setting the given bits,
  within a bit range (defaults to a full range of [8 0])"
  ([reg-addr bits-to-set] (set-register-bits reg-addr bits-to-set [8 0]))
  ([reg-addr bits-to-set [b1 b0]]
   (let [old-val (read-register reg-addr)
         mask (->> (map-indexed (fn [idx n] 
                                  (let [b (if (>= b1 idx b0) 0 1)]
                                    (bit-shift-left b n)))
                                (range 8))
                   reverse
                   (reduce bit-or))
         bits-to-set (or (and (= b1 8) bits-to-set) (bit-shift-left bits-to-set b0))
         new-val (bit-or (bit-and old-val mask) bits-to-set)
         _ (println (format "Addr %02x" reg-addr) 
                    "mask" (->bits mask) "set" (->bits bits-to-set) "from" (->bits old-val) "to" (->bits new-val))]
     (i2c-write board i2c-address reg-addr [new-val])
     [old-val new-val])))


(defn- reg->addr-coords
  "Extract the address and bit slice values for a given pseudo register name"
  [name]
  (for [[rn slice] (registers name)
      :let [[addr] (registers rn)]]
  [rn addr slice]))

(defn soft-reset!
  "reset registers to default values by writing a 1 to COM7[7] (0x12)"
  []
  (i2c-write board i2c-address 0x12 [0x80]))

(defmacro read-or-set-register
  "Read or change a given register setting."
  [reg & [value]] 
  `(do (when ~value
         (let [[addr#] (registers ~reg)]
           (set-register-bits addr# ~value)))
       (register ~reg)))

(defmacro read-or-set-pregister
  "Read or change a given `pseudo-register` setting."
  [pseudo-reg & [value]] 
  `(do (when ~value
         (let [[[_# addr# slice#]] (reg->addr-coords ~pseudo-reg)]
           (set-register-bits addr# ~value slice#)))
       (register ~pseudo-reg)))

(defn enable-scaling
  "Read or change the scaling mode.  Passing `true` enables scaling when
  using predefined format (defined via COM7[5:3]), or to enable manual
  scaling.  For manual scaling then COM14[3] must also be set to 1."
  [& [enable]]
  (read-or-set-pregister :enable-scaling (case enable true 1 false 0 nil)))

(defn free-running-pixel-clock
  "Read or change the free-running pixel clock setting. Passing `true`
  enables the free-running pixel clock, and `false` disables
  it (i.e. set the bit value to 1, so that the pixel clock will not
  toggle during horizontal blanking periods)."
  [& [enable]]
  (read-or-set-pregister :free-running-pixel-clock (case enable true 0 false 1 nil)))

(defn output-format-range
  "Read or change the output format range."
  [& [output-range]]
  (let [rng (condp = output-range
                :x10-0xF0 2r00
                :x01-0xFE 2r10
                :x00-0xFF 2r11
                nil)]
    (read-or-set-pregister :output-format-range rng)))

(defn pll-multiplier
  "Read or adjust the PLL multiplier. Pass a value between 0 and 3 to
  change the PLL-Multiplier value:

  0 00: Bypass PLL
  1 01: Multiply input clock by 4
  2 10: Multiply input clock by 6
  3 11: Multiply input clock by 8"
  [& [level]] 
  (read-or-set-pregister :pll-multiplier level))

(defn pixel-format
  "From section 2 of the implementation guide, the OV7670 has an image
  array size of 656 columns by 488 rows (320,128) pixels.  The pixel
  cells themselves are identical, but have RGB color filters arranged
  to inerpolate each pixels' BG or GR color from the light striking
  the cell directly, as well as from the light striking the
  surrounding cells.  The 'Raw Bayer RGB' image does not have any
  image processing.

  The output pixel configuration is specified via 4 bits taken from
  the COM7 and COM15 registers:

  Format               |  COM7[2] | COM7[0] | COM15[5] | COM15[4]                     
  ---------------------+----------+---------+----------+---------
  Raw Bayer RGB        |     0    |    1    |    x     |    0     
  Processed Bayer RGB  |     1    |    1    |    x     |    0     
  YUV/YCbCr 4:2:2      |     1    |    1    |    x     |    0   
  GRB 4:2:2            |     1    |    0    |    x     |    0       
  RGB565               |     1    |    0    |    0     |    1
  RGB555               |     1    |    0    |    1     |    1"
  []
  (let [[_ b3 b2 b1 b0] (map first (register :pixel-format))]
    (case (+ (<<< b3 1) b2)
      2r01 :raw-bayer-rgb
      2r11 :processed-bayer-rgb
      2r00 :yuv-422
      2r10 (case (+ (<<< b1 1) b0)
             2r01 :rgb565
             2r11 :rgb555
             :grb-422))))

(defn brightness-level
  "Read or adjust the brightness level.  Passing no arguments returns
  the current value of the BRIGHT register.  This register, 0x55, uses
  bit 7 as a sign bit.  0 is positive, 1 is negative.  A value of 0x00
  or 0x80 means no brightness adjustment.

  Pass a new value to set a new register value."
  [& [level]] 
  (read-or-set-register "BRIGHT" level))

(defn contrast-level
  "Read or adjust the contrast level.  Passing no arguments returns the
  current value of the CONTRAS register.  The bigger the value, the
  higher the contrast.  A value of 0x40 means no contrast adjustment.

  Pass a new value to set a new register value."
  [& [level]] 
  (read-or-set-register "CONTRAS" level))

(defn night-mode
  "Read or change the night-mode setting.  Passing no arguments returns
  the current mode, otherwise passing `true` or `false` enables or
  disables night mode."
  [& [enable]] 
  (read-or-set-pregister :night-mode (case enable true 1 false 0 nil)))

(defn sharpness-mode
  "Read or adjust the sharpness mode.  Passing no arguments returns the
  current sharpness mode, otherwise passing `true` or `false` enables
  or disables automatic sharpness mode."
  [& [auto]] 
  (read-or-set-pregister :sharpness-mode (case auto true 1 false 0 nil)))

(defn sharpness-level
  "Read or adjust the sharpness level.  Passing no arguments returns the
  current sharpness mode and value. Sharpness upper and lower limits
  are used in the automatic sharpness mode.

  Pass a new sharpness value to override automatic sharpness adjustment"
  ([] {:mode  (register :sharpness-mode)
       :level (register :sharpness)
       :ul    (register :sharpness-ul)
       :ll    (register :sharpness-ll)})
  ([adj] (let [[[_ addr slice]] (reg->addr-coords :sharpness)]
           (sharpness-mode false)
           (set-register-bits addr adj slice)
           (dissoc (sharpness-level) :ul :ll))))


(defn exposure-mode
  "Read or adjust the exposure mode.  Passing no arguments returns the
  current exposure mode, otherwise passing `true` or `false` enables
  or disables automatic exposure control mode."
  [& [auto]] 
  (read-or-set-pregister :aec-mode (case auto true 1 false 0 nil)))


(defn exposure
  "Get the current exposure time value. 

  Exposure time is the row interval time multiplied by the automatic
  exposure control AEC value.  

  AEC[15:0] is stored in three registers, AECHH[5:0], AECH[7:0], and
  COM1[1:0].

  Exposure time represents increments of row interval time, which is
  computed as:

     (* 2 internal-clk (+ 784 dummy-pixels))

  A single row interval time unit, at 8mhz clock (2 us per cycle) is
  3.136 ms."

  ([]
   (let [[_ AECHH AECH COM1] (map first (register "AEC"))
         aec (+ (<<< AECHH 10) (<<< AECH 2) COM1)]
     [aec (format-micros (* aec (* 2 2 784)))]))
  ([value]
   (exposure-mode false)
   (let [[[_ r1-addr r1-slice] [_ r2-addr r2-slice] [_ r3-addr r3-slice]] (reg->addr-coords "AEC")]
     (set-register-bits r1-addr (>>> value 10) r1-slice)
     (set-register-bits r2-addr (>>> (bit-and value 0x3FC) 2) r2-slice)
     (set-register-bits r3-addr (bit-and value 0x03) r3-slice)
     (exposure))))

(defn exposure-algorithm
  "There are two exposure algorithms, average based (:avg) and
  histogram (:hist) based. Pass no arguments to read the current value
  or one of :avg or :hist to change it."
  [& [algo]]
  (let [[_ [value]] (read-or-set-pregister :aec-algorithm (case algo :avg 0 :hist 1 nil))]
    (case value
        0 :avg
        1 :hist)))

(defn gain-mode
  "Read or set the automatic gain control.  Passing `true` or `false`
  enables or disables automatic gain control."
  [& [auto]] 
  (read-or-set-pregister :agc-mode (case auto true 1 false 0 nil)))

(defn mirror
  "Read or set the horizontal mirroring scan direction.  Passing `true`
  enables horizontal mirroring, `false` disables it"
  [& [enable]] 
  (read-or-set-pregister :mirror (case enable true 1 false 0 nil)))

(defn flip
  "Read or set the vertical flip scan direction control.  Passing `true`
  enables vertical flip, `false` disables it"
  [& [enable]] 
  (read-or-set-pregister :flip (case enable true 1 false 0 nil)))

(defn gain-ceiling
  "Read or set the upper limit of gain value used within automatic gain
  control.

  000: 2x
  001: 4x
  010: 8x
  011: 16x
  100: 32x
  101: 64x
  110: 128x
  111: 128x"
  [& [level]]
  (let [levels [128 2r111
                64  2r110
                32  2r100
                16  2r011
                8   2r010
                4   2r001
                0   2r000]
        slevel (->> (partition 2 levels) (map reverse) flatten)
        [_ [value _]] (if (nil? level) 
                  (read-or-set-pregister :gain-ceiling)
                  (read-or-set-pregister :gain-ceiling (eval `(condp <= ~level ~@levels 2r000))))]
    (eval `(case ~value ~@slevel))))

(defn gain
  "Read or set the automatic gain control.  Passing a `level` amount
  disables the automatic gain function and sets the gain level to this value."
  ([] (let [[_ vref76 gain70] (->> (register :gain) (map first))]
        (+ (<<< vref76 8) gain70)))
  
  ([level] (let [[[_ r1-addr r1-slice] [_ r2-addr r2-slice]] (reg->addr-coords :gain)]
             (gain-mode false)
             (set-register-bits r1-addr (>>> level 8) r1-slice)
             (set-register-bits r2-addr (bit-and level 0xFF) r2-slice)
             (gain))))

(defn de-noise-mode
  "Read or enable/disable the automatic de-noise function.  Passing
  `true` or `false` enables or disables the function.  When enabled,
  the computed noise threshold is stored in DNSTH[7:0]."
  [& [enable]] 
  (read-or-set-pregister :de-noise-mode (case enable true 1 false 0 nil)))

(defn de-noise
  "Read the current de-noise threshold and offset values.  Passing a new
  threshold value disables automatic de-noise and set the threshold to
  this value."
  ([] {:mode (case (-> (register :de-noise-mode) fnext first) 0 :manual 1 :automatic)
       :threshold (first (register "DNSTH"))
       :offset   (first (register "REG77"))})
  ([threshold] (let [[addr] (registers "DNSTH")]
                 (de-noise-mode false)
                 (set-register-bits addr threshold [7 0])
                 (de-noise))))

(defn test-pattern
  "Read or change the test pattern output settings.  Use one of the following keywords 
  to adjust the pattern.

  :off         00  No test pattern output
  :shifting-1  01  Shifting 1
  :color-bar   10  8-bar color bar
  ;; :fade-gray 11 Fade to gray color bar"

  ([] (let [[_ pat0 pat1] (->> (register :test-pattern) (map first))]
        ([:off :shifting-1 :color-bar :fade-gray] (+ (<<< pat0 1) pat1))))
  
  ([pattern] (let [[p0 p1] (case pattern
                             :off        [0 0]
                             :shifting-1 [0 1]
                             :color-bar  [1 0]
                             :fade-gray  [1 1])
                   [[_ p0-addr p0-slice] [_ p1-addr p1-slice]] (reg->addr-coords :test-pattern)]
             (set-register-bits p0-addr p0 p0-slice)
             (set-register-bits p1-addr p1 p1-slice)
             (test-pattern))))

(defn banding-filter-mode
  "Read or change the banding-filter-mode setting.  Use `true` to enable
  or `false` to disable the banding filter.  When the filter is
  disabled, then exposure time can be any value.  When it's enabled,
  it must be N/100 (50hz) or N/120 (60hz)."
  [& [enable]] 
  (read-or-set-pregister :banding-filter-mode (case enable true 1 false 0 nil)))

(defn banding-exposure-limit
  "Read or change the banding-exposure-limit setting.  This changes the
  banding filter behavior in strong light conditions when the banding
  filter is enabled. Use `true` to enable or `false` to disable the
  banding exposure limt.  When the limit is disabled, exposure time
  will be limited to 1/100 or 1/120 second in any light condition when
  the banding filter is enabled.  When the limit is enabled, it will
  allow exposure times of less than 1/100 or 1/120 second in strong
  light conditions"
  [& [enable]] 
  (read-or-set-pregister :banding-exposure-limit (case enable true 1 false 0 nil)))

(defn banding-filter
  "Read or change the banding-filter settings.
  
  From the implementation guide, in 50 or 60 hz light, the exposure
  time must be a multiple of the flicker interval to avoid banding
  shown on the image.  The exposure time must be N/100 for 50hz light,
  and N/120 for 60hz light where N is some positive integer.

  Since the expoure time AEC[15:0] is based on row interval, AEC needs
  to know how many rows 1/100 or 1/120 second represents.  Banding
  filter registers BD50ST and BD60ST hold these values.  Compute the
  values as:
     60hz: (/ 1 (* 120 row-interval-time))
     50hz: (/ 1 (* 100 row-interval-time))

  Row-interval-time is (/ (:end @accum) number-of-rows)

  The following may or may not be a valid way to compute the banding
  filter value.

  The frame time can be measured from (@accum :end).  A
  typical value is 3.746456 seconds for qqvga, 8mhz clock, with the
  clock pre-scaler set to 11.

  There are 120 rows in qqvga, thus rows per second are:
  (/ 1 (/ 3.75 120)) 

  And rows per 1/120 second: 
  (/ 1 (/ 3.75 120) 120)

  Multiply by 100 to get a value for N in the range 0 < N < 255

  (Math/round (* 100 (/ 1 (/ 3.75 120) 120)))"
  ([] {:bd50 (register "BD50ST") :bd60 (register "BD60ST")})
  ([auto] (let [[addr50] (registers "BD50ST")
                [addr60] (registers "BD60ST")
                ftime (or (@accum :end) 3746456)
                n50 (Math/round (* 100000000 (/ 1 (/ ftime 120) 100.0)))
                n60 (Math/round (* 100000000 (/ 1 (/ ftime 120) 120.0)))]
            (banding-filter-mode true)
            (set-register-bits addr50 n50 [7 0])
            (set-register-bits addr60 n60 [7 0])
            (banding-filter))))

(defn color-matrix
  "This matrix applies color correction for the U & V channels.  The 
  Y channel is not altered by the matrix settings.

  From the implementation guide, it says the color matrix is used to
  eliminate the cross talk induced by the micro-lens and color filter
  process. It also compensates for lighting and temperature effects.
  
  The Y signal is not from the color matrix.  The sensor generates the
  Y signal from the original RGB directly.  The color matrix performs
  the color correction, RGB to YUV/YCbCr conversion, hue and color
  saturation control.  Though the Y signal is not from the color
  matrix, the calculation should be done by the 3x3 matrix to get the
  combined matrix as shown below:

  CombinedMatrix = SatMatix x HueMatrix x ConversionMatrix x CorrectionMatrix

  and then take the two rows for UV/CbCr as the final color matrix.

  RGB to YUV Conversion Matrix can be derived from the standard
  equations below:

  Y = 0.59G + 0.31R + 0.11B
  U = B - Y
  V = R - Y
  Cr = 0.713 (R-Y)
  Cb = 0.563 (B-Y)

  Each matrix element has 9 bits, 1 sign bit and 8 data bits.  The
  register value equals up to 128 times the real color matrix value."
  ([]
   (let [[[_ mtsx-addr mtsx-slice]] (reg->addr-coords :color-matrix-sign-bits)
         [_ [signs _]] (register :color-matrix-sign-bits)
         signs (for [i (range 5 -1 -1) :let [sign (or (and (bit-test signs i) -1) 1)]] sign)
         cm (mapv (comp first register #(str "MTX" %)) (range 1 7))]
     [(mapv #(format "%02X" %) cm) (mapv * cm signs)]))

  ([matrix]
   (let [[[_ mtsx-addr mtsx-slice]] (reg->addr-coords :color-matrix-sign-bits)
         [_ [signs _]] (register :color-matrix-sign-bits)
         regs (mapv (comp first registers #(str "MTX" %)) (range 1 7))
         signs (->> (map #(if (pos? %) 0 1) matrix) reverse (apply str "2r") read-string)
         matrix (mapv #(Math/abs %) matrix)]
     (doall (for [i (range 6)] (set-register-bits (regs i) (matrix i) [7 0])))
     (set-register-bits mtsx-addr signs mtsx-slice)
     (color-matrix))))

(defn- gamma-coordinates
  "Fetch the gamma curve segment coordinates"
  []
  (let [regs (map #(str "GAM" %) (range 1 16))
        xref [4 8 16 32 40 48 56 64 72 80 96 112 144 176 208]]
    (->> (for [x (range 1 16)
               :let [xr (xref (dec x))
                     reg (str "GAM" x)
                     [addr] (registers reg)
                     y   (first (register reg))]]
           {reg {:addr addr :x xr :y y}})
         (reduce into))))

(defn gamma
  "Read or adjust the gamma curve values.  It takes one argument
  `gamma-y` that is used as the exponent in a function that computes
  new y coordinate segment values for each xref position.  The OV7670
  uses a slope value to approximate the gamma between xref 208 and
  255.  This slope value is computed from the GAM15 value."
  ([] (-> (reduce-kv (fn [m k v] (assoc m k (dissoc v :addr) )) {} (gamma-coordinates))
          (assoc "SLOP" (first (register "SLOP")))))
  ([gamma-y]
   (let [fx #(-> % (/ 255) (Math/pow gamma-y) (* 255) (Math/round) int)
         coords  (gamma-coordinates)
         slope   (->> (coords "GAM15") :x fx (- 256) (* (/ 4 3)) int)
         [saddr] (registers "SLOP")]
     (doall (for [i (keys coords)
                  :let [{:keys [addr x]} (coords i)
                        y (fx x)]]
              (set-register-bits addr y [7 0])))
     (set-register-bits saddr slope [7 0]))
   (gamma)))

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
        reg (read-register com14)]
    (i2c-write board i2c-address com14 [(bit-or reg 2r1100)]))


  ;; try enabling digital zoom and down sampling
  (let [com03 0x0C
        reg (read-register com03)]
    (i2c-write board i2c-address com03 [(bit-or reg 2r1100)]))

  ;; down sample v&h by 4
  (let [register 0x72
        reg (read-register register)]
    (i2c-write board i2c-address register [(bit-or reg 2r100010)]))

  (let [register 0x73
        reg (read-register register)]
    (i2c-write board i2c-address register [(bit-or reg 2r1010)])))



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
  (sorted-map ;;:x11 (set-register-bits 0x11 0x01) ;; 2us per cycle
;;              :x11 (set-register-bits 0x11 0x05) ;; 6us per cycle
;;              :x11 (set-register-bits 0x11 0x08) ;; 9us per cycle
              :x11 (set-register-bits 0x11 0x11) ;; 18us per cycle
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
  (free-running-pixel-clock false)
;;  (cmd :clock-at-1mhz)
  "ok")

(comment
  @accum
  (open-board)
  (close board)
  (initialize)
  (i2c-init board)
  (qqvga!)
  (soft-reset!)
  (exposure)
  ;; pclk pre-scaller
  (set-register-bits 0x11 10)
  ;; MSB, dummy pixel insert
  (set-register-bits 0x2A 0x00 [7 4])
  (set-register-bits 0x2B 0x00 [7 0])
  ;; MSB, dummy lines
  (set-register-bits 0x2E 0x00 [7 0])
  (set-register-bits 0x2D 60 [7 0])
  (cmd :capture-image)  
  (cmd :test)
  (cmd :first-pulse)
  (cmd :clock-at-1mhz)
  (cmd :clock-at-8mhz)
  (cmd :vsync-timing)
  ;; [msb, lsb] pc ms delay; pc us
  (doseq [n (range 1 10)]
    (cmd :timer-delay 0 27 0 n)
    (Thread/sleep 1000)
    (cmd :some-timing))

  (doseq [n (range 500 600 10)]
    (cmd :timer-delay 0 143 n 0)
    (Thread/sleep 500)
    (cmd :some-timing))
  (cmd :count-pixels)
  (cmd :pckl-timing)
  (cmd :test))


(comment 
  (i2c-init board)
  (close board)
  (read-register 1)
  ;; video format
  (read-register 0x12)
  (enable-scaling true)
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
