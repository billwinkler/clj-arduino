(ns clj-arduino.hx711
  (:use :reload-all clj-arduino.core)
  (:use :reload-all clodiuno.core)
  (:use :reload-all clodiuno.firmata)
  (:require [clojure.core.async
             :as a
             :refer [>! <! >!! <!! go chan alts!! close! timeout]]))

(def parameters (atom {:tare 268207711  ;; offset when scale is clear
                       :calib 450 ;; calibration factor
                       :queue-depth 10
                       :noise-threshold 50 ;; see docs
                       :noise-level 80 
                       :debug true}))
(def message-channels (atom {}))
(def state-machine (atom {:state :idle :scale :unknown}))
(def queue (ref clojure.lang.PersistentQueue/EMPTY)) ;; measurements

(def ^:dynamic *channel* (chan (a/sliding-buffer 1)))

(defn get-next
  "try to pull a measurement off the measurement channel.
  return null if nothing is read in the next x msecs   "
  ([] (get-next 500))
  ([msecs] (let [[reading _] (alts!! [*channel* (timeout msecs)])]
             (if reading reading {}))))

(defn readings
  "the measurement history up to queue-depth"
  ([] (seq @queue))
  ([n] (take n (queue))))

(defn avg
  "compute average of the rolling history"
  [readings]
  (if (zero? (count readings)) 0
      (/ (reduce + readings) (count readings))))

(defn- fmt-float 
  "truncate trailing digits"
  [f]
  (format "%.1f" (float f)))

(defn running?
  "true while readings are being taken"
  []
  (not= :done (:state @state-machine)))

(defn trend-analytics
  "Compare first half measurements to most recent half"
  []
  (let [noise (:noise-threshold @parameters) ;; threshold for random variance
        nl (:noise-level @parameters) ;; threshold for random variance 
        n (int (/ (:queue-depth @parameters) 2))
        rng (map - (rest (readings)) (readings))
        mnv (if (empty? rng) 0 (apply min rng))
        mxv (if (empty? rng) 0 (apply max rng))
        segs (partition-all n (readings))
        [t0 t1] (if (> 2 (count rng)) [0 0] (map avg segs))
        dir (cond
              (or (< mnv (- nl)) (> mxv nl)) :noise
              (> noise (Math/abs (float (- t1 t0)))) :stable
              (> t1 t0) :inc
              :else :dec)]
    (hash-map :t0 t0 :t1 t1 :segs segs :dir dir
              :bias (reduce + rng) :mnv mnv :mxv mxv)))

(defn stable?
  "true when last 10 measurements are within +/- some tbd percent"
  [])

(defn measure
  "average of last n readings"
  ([] (measure 5))
  ([n] (letfn [(scale [m] (/ (- m (:tare @parameters)) (:calib @parameters)) )]
         (->> (readings) reverse (take n) avg float
              scale fmt-float read-string))))

(defn tare!
  "reset the tare value"
  ([] (tare! (->> (readings) avg float)))
  ([value] (:tare (swap! parameters assoc-in [:tare] value))))

(defn calibrate!
  "compute the measurement scale"
  ([] (calibrate! 25))
  ([ref-weight]
   (let [reading (:t1 (trend-analytics))
         scale (float (/ (- reading (:tare @parameters)) ref-weight))]
     (:calib (swap! parameters assoc-in [:calib] scale)))))

(defn set-queue-depth!
  "set a new queue-depth"
  [size]
  (:queue-depth (swap! parameters assoc-in [:queue-depth] size)))


(defn printer
  [in]
  (let [out (chan (a/sliding-buffer 10))]
    (go (while (running?)
          (when-let [reading (<! in)]
            (when (:debug @parameters) (println reading))
            (a/put! out reading))))
    out))

(defn push!
  "push an item onto the queue"
  [item]
  (when (= (:queue-depth @parameters) (count @queue)) (dosync (alter queue pop)))
  (dosync (alter queue conj item)))

(defn q-handler
  [in]
  (let [out (chan)]
    (go (while (running?)
          (let [reading (<! in)]
            (push! reading)
            (when (:debug @parameters) (println "qh>" @queue))
            (a/put! out reading))))
    out))

(defn analytics-handler
  [in]
  (let [out (chan)]
    (go (while (running?)
          (let [reading (<! in)
                an (trend-analytics)]
            (when (:debug @parameters)
              (println "ah>" (measure) (:dir an) :bias (:bias an) [(:mnv an) (:mxv an)]))
            (a/put! out reading))))
    out))

(defn reading-handler
  "decode the next n incoming readings and then quit"
  [n]
  (let [in (chan)
        out (chan (a/dropping-buffer 10))]
    (go (loop [mc n] ;; message count
          (if (> mc 0)
            (let [msg (<! in)]
              (do (>! out (decode-msg msg))
                  (recur (dec mc))))
            (do (close! in)
                (close! out)
                (swap! state-machine assoc-in [:state] :done)))))
    [in out]))

(defn measurement-handler
  [in]
  (let [out (chan)
        lst (atom 0)]
    (go (while (running?)
          (when (> (count (readings)) 1)
            (let [reading (<! in)
                  st (-> (trend-analytics) :dir)
                  m (if (= :stable st) (measure) nil)]
              (when (and m (not= m @lst) (and (< 0.15 (Math/abs (- m @lst)))))
                (reset! lst m)
                (a/put! *channel* {:weight m :raw reading})
                (println "mh>" m))
              (a/put! out reading)))))
    out))


(defn message-handler
  "role is to push decoded incoming values onto a queue"
  [limit]
  (let [[in out] (reading-handler limit)]
    (reset! message-channels {:in in :out out})
    (swap! state-machine assoc-in [:state] :begin)
    (fn [msg]
      ;; https://clojure.org/guides/core_async_go#_general_advice
      (go (a/put! in msg)))))


(defn state-change-alert
  "a var watcher to detect shutdown event"
  [board]
  (fn 
    [key watched old-state new-state]
    (let [old (:state old-state)
          new (:state new-state)]
      (println key ":" old "->" new)
      (when (not= old new)
        (condp = new
          :running (println "begin running")
          :done (do (println "exiting") (close board))
          (println "just fyi.." old "->" new))))))

(defn start!
  "start running"
  [n]
  (let [board (arduino :firmata (arduino-port) :msg-callback (message-handler n))]
    (do
      (-> (printer (:out @message-channels))
          (q-handler)
          (analytics-handler)
          (measurement-handler))
      (add-watch state-machine :state-change-alert (state-change-alert board))
      (swap! message-channels assoc-in [:board] board))
    nil))

(defn stop!
  []
  (swap! state-machine assoc-in [:state] :done))

(comment
  (start! 300)
  (get-next)
  (<!! channel)
  (tare!)
  (calibrate!)
  (set-queue-depth! 10)
  (stop!)
  (remove-watch state-machine :state-change-alert)
  (swap! state-machine assoc-in [:state] :done)

  (go (while (running?)
        (println "ch" (<!! *channel*))))

  )
