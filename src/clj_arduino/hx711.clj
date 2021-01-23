(ns clj-arduino.hx711
  (:use :reload-all clj-arduino.core)
  (:use :reload-all clodiuno.core)
  (:use :reload-all clodiuno.firmata)
  (:require [clojure.core.async
             :as a
             :refer [>! <! >!! <!! go chan buffer close! thread
                     alts! alts!! timeout]]))

(def parameters (atom {:tare 268207711  ;; offset when scale is clear
                       :calib 450 ;; calibration factor
                       :tare-threshold 268205000 ;; to determine if the scale is clear
                       :queue-depth 10
                       :debug false}))
(def message-channels (atom {}))
(def state-machine (atom {:state :idle :scale :unknown}))
(def readings (ref clojure.lang.PersistentQueue/EMPTY))

(type readings)

(defn- avg
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

(defn you-need-more-than-one-measurement-for-this-to-work
  []
  (> (count @readings) 1))

(defn trend-analytics
  "Compare first half measurements to most recent half"
  []
  {:pre [(you-need-more-than-one-measurement-for-this-to-work)]}
  (let [noise 50 ;; threshold for random variance 
        n (int (/ (:queue-depth @parameters) 2))
        rng (map - @readings (rest @readings))
        mnv (apply min rng)
        mxv (apply max rng)
        segs (partition-all n @readings)
        [t0 t1] (map avg segs)
        dir (cond
              (or (< mnv -80) (> mxv 80)) :noise
              (> noise (Math/abs (float (- t1 t0)))) :level
              (> t1 t0) :up
              :else :down)]
    (hash-map :t0 t0 :t1 t1 :segs segs :dir dir
              :bias (reduce + rng) :mnv mnv :mxv mxv)))

(defn stable?
  "true when last 10 measurements are within +/- some tbd percent"
  [])

(defn measure
  "average of last n readings"
  ([] (measure 5))
  ([n] (letfn [(scale [m] (/ (- m (:tare @parameters)) (:calib @parameters)) )]
         (->> @readings reverse (take n) avg float
              scale fmt-float read-string))))

(defn tare!
  "reset the tare value"
  [] 
  (let [tare (->> @readings avg float)]
    (:tare (swap! parameters assoc-in [:tare] tare))))

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
          (let [reading (<! in)]
            (when (:debug parameters) (println reading))
            (a/put! out reading))))
    out))

(defn push!
  "push an item onto the queue"
  [item]
  (when (= (:queue-depth @parameters) (count @readings)) (dosync (alter readings pop)))
  (dosync (alter readings conj item)))

(defn q-handler
  [in]
  (let [out (chan)]
    (go (while (running?)
          (let [reading (<! in)]
            (push! reading)
            (when (:debug parameters) (println "qh>" @readings))
            (a/put! out reading))))
    out))

(defn analytics-handler
  [in]
  (let [out (chan)]
    (go (while (running?)
          (let [reading (<! in)
                ;; this verbose function name is so that exception messages will be more clear
                an (if (you-need-more-than-one-measurement-for-this-to-work)
                     (trend-analytics)
                     {})]
            ;;(println "ah>" (measure) (:dir an) :bias (:bias an) [(:mnv an) (:mxv an)])
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
          (let [reading (<! in)
                st (-> (trend-analytics) :dir)
                m (if (= :level st) (measure) nil)]
            (when (and m (not= m @lst) (and (< 0.2 (Math/abs (- m @lst)))))
              (reset! lst m)
              (println "mh>" m))
            (a/put! out reading))))
    out))


(defn message-handler
  "role is to push decoded incoming values onto a queue"
  [limit]
  (let [[in out] (reading-handler limit)]
    (reset! message-channels {:in in :out out})
    (swap! state-machine assoc-in [:state] :begin)
    (fn [msg]
      (go (a/put! in msg)) ;; https://clojure.org/guides/core_async_go#_general_advice
;;      (go (when-let [value (<!! out)] (println value)))
      )))


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
  (start! 200)
  (tare!)
  (calibrate!)
  (set-queue-depth! 10)
  (stop!)
  (remove-watch state-machine :state-change-alert)
  (swap! state-machine assoc-in [:state] :done))



