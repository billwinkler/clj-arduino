(ns clj-arduino.hx711
  (:use :reload-all clj-arduino.core)
  (:use :reload-all clodiuno.core)
  (:use :reload-all clodiuno.firmata)
  (:require [clojure.core.async
             :as a
             :refer [>! <! >!! <!! go chan alts!! close! timeout pipeline]]))

(def parameters (atom {:tare 268206576  ;; offset when scale is clear
                       :calib 457.58 ;; calibration factor
                       :queue-depth 10
                       :noise-threshold 50 ;; see docs
                       :noise-level 80 
                       :debug #{:printer}}))

(def message-channels (atom {}))
(def state-machine (atom {:state :idle}))
(def queue (ref clojure.lang.PersistentQueue/EMPTY)) ;; measurements

(def ^:private scale> (chan (a/sliding-buffer 1)))
(def ^:private readings> (chan (a/sliding-buffer 10)))
(def ^:private kill> (chan))

(defn get-next
  "try to pull a measurement off the measurement channel.
  return null if nothing is read in the next x msecs   "
  ([] (get-next 500))
  ([msecs] (let [[reading _] (alts!! [scale> (timeout msecs)])]
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

(defn debug!
  "Clear debug flags by default, or set them to some elements"
  [& flags] (:debug (swap! parameters assoc-in [:debug] (set flags))))

(defn debug?
  "test debug settings"
  [some-key]
  (contains? (:debug @parameters) some-key))

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

(defn push!
  "push an item onto the queue"
  [item]
  (when (= (:queue-depth @parameters) (count @queue)) (dosync (alter queue pop)))
  (dosync (alter queue conj item)))

(defn queuer
  "push the next reading onto the queue"
  [{:keys [raw] :as msg}]
  (when-let [reading raw] (push! reading))
  (when (debug? :queuer) (println "qh>" @queue))
  msg)

(defn analyzer
  "apply trend-analytics to the incoming message"
  [{:keys [raw] :as msg}]
  (let [msg (merge msg (trend-analytics))]
    (when (debug? :analyzer)
      (let [ {:keys [t1 t0]} msg
            fmt (fnil fmt-float 0.0)
            t1 (fmt t1)
            t0 (fmt t0)
            delta (- (read-string t1) (read-string t0))]
        (println "ah>" raw (:dir msg) :bias (:bias msg) 
                 :mn-mx [(:mnv msg) (:mxv msg)]
                 :t0-t1 [t0 t1]
                 :delta delta)))
    msg))


(defn decoder
  "take a 4 character sysex string, 
  output in a map as the raw decoded integer value"
  [sysex-string]
  (hash-map :raw (decode-msg sysex-string)))

(defn de-dupper
  "filter out readings that are essentially the same as the last reading"
  ([] (de-dupper 500))
  ([delta]
   (let [last-raw (atom 0)]
     (fn [{:keys [raw weight] :as msg}]
       (let [diff (Math/abs (- raw @last-raw))]
         (when (debug? :de-dupper)
           (println "dd>" :from @last-raw :to raw
                    :delta [diff ":" delta]
                    (if (< diff delta) :drop :keep)
                    :weight weight))
         (cond 
           (nil? raw) msg    ;; simply pass it along
           (> delta diff) {} ;; ignore it
           :else (do (reset! last-raw raw) msg)))))))

(defn weight-assessor
  "compute weight value if stable trend"
  [{:keys [raw dir] :as msg}]
  (cond
    (> 1 (count (readings))) msg ;; not enough readings yet
    (nil? raw) msg ;; duplicate
    (not= dir :stable) msg ;; pass it on if not yet stable
    :else (assoc msg :weight (measure))))

(defn trimmer
  "prune down the measurement event size"
  [msg]
  (dissoc msg :segs))

(defn printer
  "print some elements of the reading payload"
  [{:keys [dir raw weight] :as msg}]
  (when (and (not-empty msg)
             (debug? :printer))
    (println dir raw weight))
  msg)

(defn state-change-alert
  "a var watcher to detect shutdown event"
  [board]
  (fn 
    [key watched old-state new-state]
    (let [old (:state old-state)
          new (:state new-state)]
      (when (not= old new)
        (condp = new
          :running (println "begin running")
          :done (do (println "exiting") (close board))
          (println "state change:" old "->" new))))))

(defn msg-handler
  "generate a callback function; set state machine to :init "
  []
  (swap! state-machine assoc-in [:state] :init)
  (fn [msg] (go (a/put! readings> msg))))

(defn start!
  "start running"
  []
  (let [board (arduino :firmata (arduino-port) :msg-callback (msg-handler))
        _ (swap! state-machine assoc-in [:state] :running)
        xform (comp
               (map decoder)
               (map queuer)
               (map analyzer)
               (map weight-assessor)
               (map (de-dupper 50))
               (filter (fn [{:keys [raw dir]}] (= dir :stable)))
               (map printer)
               (map trimmer))
        _ (pipeline 1 scale> xform readings>)] 
    (do
      (add-watch state-machine :state-change-alert (state-change-alert board))
      (swap! message-channels assoc-in [:board] board))
    "ok"))

(defn stop!
  "Change state machine state to done"
  []
  (swap! state-machine assoc-in [:state] :done))

(comment
  (start!)
  (stop!)
  (close! readings>)
  (close! scale>)
  (get-next)
  (<!! channel)
  (tare!)
  (calibrate!)
  (set-queue-depth! 10)
  (a/poll! readings>)
  (a/poll! scale>)
  (debug!)
  (debug! :printer)
  (debug! :queuer)
  (debug! :de-dupper)
  (debug! :analyzer)
  
  (remove-watch state-machine :state-change-alert)
  (swap! state-machine assoc-in [:state] :done)


  )
