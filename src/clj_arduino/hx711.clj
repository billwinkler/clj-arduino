(ns clj-arduino.hx711
  (:use :reload-all clj-arduino.core)
  (:use :reload-all clodiuno.core)
  (:use :reload-all clodiuno.firmata)
  (:require [clojure.core.async
             :as a
             :refer [>! <! >!! <!! go chan buffer close! thread
                     alts! alts!! timeout]]))

(def parameters (atom {:tare 0  ;; offset when scale is clear
                       :calib 1 ;; calibration factor
                       :tare-threshold 268205000 ;; to determine if the scale is clear
                       :queue-depth 10}))
(def message-channels (atom {}))
(def state-machine (atom {:state :idle :scale :unknown}))
(def readings (ref clojure.lang.PersistentQueue/EMPTY))


(defn- avg
  "compute average of the rolling history"
  [hist]
  (if (zero? (count hist)) 0
    (/ (reduce + hist) (count hist))))

(defn- fmt-float 
  "truncate trailing digits"
  [f]
  (format "%.1f" (float f)))

(defn calibration-factor
  "compute the measurement scale"
  [ref-weight tare reading]
  (float (/ (- reading tare) ref-weight)))

(defn running?
  "test current state"
  []
  (not= :done (:state @state-machine)))

(defn printer
  [in]
  (let [out (chan (a/sliding-buffer 10))]
    (go (while (running?)
          (let [reading (<! in)]
            (println reading)
            (a/put! out reading))))
    out))

(defn push!
  "push an item onto the queue"
  [item]
  (when (= (:queue-depth @parameters) (count @readings)) (dosync (alter readings pop)))
  (dosync (alter readings conj item)))

(defn q-handler
  [in]
  (go (while (running?) (push! (<! in)) (println "qh>" (peek @readings)))))

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

;;(def msg-callback (message-handler))


#_(def msg-callback
  (let [debug false
        qd 15  ;; queue depth for averaging tare value
        ci 100 ;; calibration interval
        n (atom 0)
        tare-threshold 268205000 ;; assume scale is not cleared yet
        state (atom :init)
        hist (ref clojure.lang.PersistentQueue/EMPTY)
        tare (atom 0)
        calib (atom 1)
        last-reading (atom 0)
        weight (atom 0)
        delta-threshold 75
        take-reading (fn [value] (when (> (count @hist) 5)
                              (cond (< delta-threshold (Math/abs (- @last-reading value)))
                                    (do (reset! last-reading value)
                                        (repeatedly (count @hist) #(dosync (alter hist pop)))
                                        (println (fmt-float @weight) value)))))]
    (fn [msg]
      (swap! n inc)
      (let [value (decode-msg msg)]
        (when (= qd (count @hist)) (dosync (alter hist pop)))
        (dosync (alter hist conj value))
        (reset! weight (/ (- (avg @hist) @tare) @calib))

        (condp = @state
          :init       (cond (> value tare-threshold)
                            (do (println "initializing" value)
                                (reset! state :tare-wait))
                            :else (reset! state :tare-begin))
          :tare-wait  (cond (< value tare-threshold) (reset! state :tare-begin)
                            (zero? (mod @n 10)) (println "clear the scale!" value))
          :tare-begin (do (reset! n 0) (reset! state :tare-running))
          :tare-running (when (> @n qd)
                          (reset! tare (avg @hist))
                          (println "tare value is:" (fmt-float @tare))
                          (reset! state :calib-begin))
          :calib-begin (do (reset! n 0) (reset! state :calib-wait)
                           (println "place 25g reference weight on scale..."))
          :calib-wait (cond (> (- value @tare) 1000)
                            (do (reset! state :calib-pend)
                                (reset! n 0)
                                (println "weight detected..." value (fmt-float @tare) (- value @tare)))
                            (zero? (mod @n 50)) (println "waiting for calibration weight!" value))
          
          :calib-pend (when (> @n 10) (do (reset! state :calibrating) (reset! n 0)))
          :calibrating (when (> @n 10) (do (reset! state :running) (reset! n 0)
                                           (reset! calib (calibration-factor 25.0 @tare value))
                                           (println "calibration factor set" (fmt-float @calib))))
          :running (take-reading value))

        (when (and debug (zero? (mod @n 10))) (println @n @state @weight))))))

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
          (q-handler))
      (add-watch state-machine :state-change-alert (state-change-alert board))
      (swap! message-channels assoc-in [:board] board))
    nil))

(defn stop!
  []
  (swap! state-machine assoc-in [:state] :done))

(comment
  (start! 20)
  (stop!)
  (remove-watch state-machine :state-change-alert)
  (swap! state-machine assoc-in [:state] :done))



