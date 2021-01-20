(ns clj-arduino.core
  (:import [gnu.io CommPortIdentifier]))

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



