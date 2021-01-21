(ns clj-arduino.core
  (:import [gnu.io CommPortIdentifier NoSuchPortException]))

(defn port-type [^CommPortIdentifier cpi]
  (case (.getPortType cpi)
    1 :serial
    2 :parallel
    3 :i2c
    4 :rs485
    5 :raw
    :undefined))

(defn port-ids []
  "Returns a map representing all port identifiers visible to the system"
  (letfn [(as-hash-map [^CommPortIdentifier cpi]
            {(.getName cpi)
             {:type (port-type cpi)
              :owner (.getCurrentOwner cpi)
              :identifier cpi}})]
    (->> (CommPortIdentifier/getPortIdentifiers) enumeration-seq
         (map as-hash-map) (reduce into))))

(defn find-arduino-ports []
  (->> (port-ids)
       (reduce-kv (fn [m k v] (if (clojure.string/includes? k "usbmodem") (assoc m k v) m))  {})))

(defn arduino-port
  "Use to get a default arduino port"
  []
  (or (when-let [port (-> (find-arduino-ports) keys last)] port)
      (throw NoSuchPortException)))


(defn >>> [v bits] (bit-shift-right (bit-and 0xFFFFFFFF v) bits))
(defn <<< [v bits] (bit-shift-left (bit-and 0xFFFFFFFF v) bits))

(defn encode
  "Simulate the arduino sysex string encoding of
  a 32bit hx711 integer reading"
  [n]
  (map (fn [s] (bit-and 0x7f (>>> n s))) (range 0 24 7)))

(defn decode
  "the input is a byte array where each pair of bytes
  encodes 7 bits of the hx711 integer reading value."
  [data]
  (->> (partition 2 data)
       (map first)
       (map-indexed (fn [i x] (bit-shift-left (bit-and x 0x0FF) (* 7 i))))
       (reduce bit-or)))

(defn decode-msg
  "The input is a string encoding of a single hx711 integer value.
   Each character encodes 7 bits of the value, in lsb order"
  [msg] (-> (map long msg) decode))

;; as a debugging aid
(defmethod print-method clojure.lang.PersistentQueue [q, w]
  ; Overload the printer for queues so they look like fish
  (print-method '<- w)
  (print-method (seq q) w)
  (print-method '-< w))





