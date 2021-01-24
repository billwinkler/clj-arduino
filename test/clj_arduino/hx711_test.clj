(ns clj-arduino.hx711-test
  (:require [clojure.test :refer :all]
            [clj-arduino.hx711 :refer :all]))

(defn- empty! []
  (dosync (alter queue empty)))

(deftest push-test
  (testing "pushing messages onto the queue"
    (empty!)
    (is (= 1 (do (push! 2020202020) (count @queue))))
    (is (= (:queue-depth @parameters) (do (dotimes [n 100](push! 2020202020)) (count @queue))))))

(deftest avg-test
  (testing "computing the average of raw messages on the queue"
    (empty!)
    (is (= 0 (avg (readings))))
    (is (= 100 (do (push! 100) (avg (readings)))))
    (is (= 150 (do (push! 200) (avg (readings)))))
    (is (=  100 (do (dotimes [n 10] (push! 100)) (avg (readings)))))))

(deftest tare-test
  (testing "resetting tare value"
    (empty!)
    (is (= 0.0 (tare!)))
    (is (= 100.0 (do (dotimes [n 10] (push! 100)) (tare!))))
    (is (= 500 (tare! 500)))
    (is (= 500 (:tare @parameters)))))

(deftest readings-test
  (testing "reading measurements"
    (empty!)
    (is (= nil (readings)))
    (is (= [1] (do (push! 1) (vec (readings)))))
    (is (= [1 0 1 2] (do (doseq [n (range 3)] (push! n)) (vec (readings)))))))

(deftest trend-analytics-test
  (letfn ([mid-point [interval]
           "middle point in the indicated segment"
           (let [ta (trend-analytics)
                 seg (condp = interval
                       0 (first (:segs ta))
                       1 (second (:segs ta)))]
             (first (drop 2 seg)))])
    (testing "with empty queue"
      (empty!)
      (let [anal (trend-analytics)]
        (are [element result] (= [element result] [element (element anal)])
          :t0 0
          :t1 0
          :mnv 0
          :mxv 0
          :bias 0
          :dir :stable)))
    (testing "rising trend case 1"  
      (empty!)
      (let [q (range 0 100 10)
            _ (doseq [n q] (push! n))
            anal (trend-analytics)]
        (are [element result] (= [element result] [element (element anal)])
          :t0 (mid-point 0)
          :t1 (mid-point 1)
          :mnv 10
          :mxv 10
          :bias (* 9 10) ;; = sum of the 9, 10 unit increments between reading pairs
          :dir :inc)))
    (testing "rising trend case 2 -- same avg t0, t1 values (50)"  
      (empty!)
      (let [q [50 50 50 50 50 10 30 50 70 90]
            deltas [0 0 0 0 -40 20 20 20 20]
            _ (doseq [n q] (push! n))
            anal (trend-analytics)]
        (are [element result] (= [element result] [element (element anal)])
          :t0 (mid-point 0)
          :t1 (mid-point 1)
          :mnv (apply min deltas)
          :mxv (apply max deltas)
          :bias (reduce + deltas)
          :dir :stable)))
    (testing "rising trend case 3 -- noise(50) > t1 - t0  (- 80 50) ==> 40"  
      (empty!)
      (let [q [50 50 50 50 50 60 70 80 90 100]
            deltas [0 0 0 0 10 10 10 10 10]
            _ (doseq [n q] (push! n))
            anal (trend-analytics)]
        (are [element result] (= [element result] [element (element anal)])
          :t0 (mid-point 0)
          :t1 (mid-point 1)
          :mnv (apply min deltas)
          :mxv (apply max deltas)
          :bias (reduce + deltas)
          :dir :stable)))
    (testing "rising trend case 4 -- noise(50) < t1 - t0  (- 110 50) ==> 60"  
      (empty!)
      (let [q [50 50 50 50 50 90 100 110 120 130]
            deltas [0 0 0 0 40 10 10 10 10]
            _ (doseq [n q] (push! n))
            anal (trend-analytics)]
        (are [element result] (= [element result] [element (element anal)])
          :t0 (mid-point 0)
          :t1 (mid-point 1)
          :mnv (apply min deltas)
          :mxv (apply max deltas)
          :bias (reduce + deltas)
          :dir :inc)))))

(deftest calibrate-test
  (testing "resetting scale factor"
    (do (empty!)
        (tare! 90)
        (push! 100))
    (is (= -100.0 (calibrate! 1)) "(/ (- reading tare 1")
    (is (= 100.0 (do (dotimes [n 10] (push! 100)) (calibrate! 1))))))

(deftest measure-test
  (testing "average of last n readings"
    (empty!)
    (let [q [0 0 0 0 0 10 20 30 40 50]
          _ (doseq [n q] (push! n))
          tare (tare! 0)
          ref-weight 1
          calib (calibrate! ref-weight)
          t1 (:t1 (trend-analytics))]
      (is (= 0 tare))
      (is (= calib (float (/ (- t1 tare) ref-weight))))    
      (is (= (Math/ceil (/ (- (last q) tare) calib)) (Math/ceil(measure 1)))))))

