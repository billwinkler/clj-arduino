(ns clj-arduino.ov7670-test
  (:require [clojure.test :refer :all]
            [clj-arduino.ov7670 :refer :all]))

(defn- parse-return
  [[label [value _]]] value)

(deftest enable-scaling-test
  (testing "reading and altering scaling mode"
    (is (= 1 (-> (enable-scaling true) parse-return)))
    (is (= 0 (-> (enable-scaling false) parse-return)))
    (is (= 0 (-> (enable-scaling) parse-return)))))

(deftest free-running-pixel-clock-test
  (testing "reading and altering free running pixel clock"
    (is (= 0 (-> (free-running-pixel-clock true) parse-return)))
    (is (= 1 (-> (free-running-pixel-clock false) parse-return)))
    (is (= 1 (-> (free-running-pixel-clock) parse-return)))))

(deftest pll-multiplier-test
  (testing "reading and altering free running pixel clock"
    (is (= 3 (-> (pll-multiplier 2r11) parse-return)))
    (is (= 0 (-> (pll-multiplier 2r00) parse-return)))
    (is (= 0 (-> (pll-multiplier) parse-return)))))
