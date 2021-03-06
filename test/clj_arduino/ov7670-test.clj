(ns clj-arduino.ov7670-test
  (:require [clojure.test :refer :all]
            [clj-arduino.ov7670 :refer :all]))

(defmacro value-of
  [fn] 
  `(let [[_# [value# _#]] ~fn]
        value#))

(defmacro value-of-1
  [fn]
  `(let [[value# _#] ~fn]
        value#))

(deftest enable-scaling-test
  (testing "reading and altering scaling mode"
    (are [result arg] (= result (value-of (enable-scaling arg)))
      1 true
      0 false
      0 nil)))

(deftest free-running-pixel-clock-test
  (testing "reading and altering free running pixel clock"
    (are [result arg] (= result (value-of (free-running-pixel-clock arg)))
      0 true
      1 false
      1 nil)))

(deftest pll-multiplier-test
  (testing "reading and altering free running pixel clock"
    (are [result arg] (= result (value-of (pll-multiplier arg)))
      3 2r11
      0 2r00
      0 nil)))

(deftest output-format-range-test
  (testing "reading and altering output format range"
    (are [result arg] (= result (value-of (output-format-range arg)))
      2r00 :x10-0xF0
      2r10 :x01-0xFE
      2r10 :bogus
      2r11 :x00-0xFF
      2r11 :bogus
      2r11 nil)))

(deftest pixel-format-test
  (testing "read current pixel format"
    (is (= :yuv-422 (pixel-format)))))

(deftest brightness-level-test
  (testing "read or change the brightness-level"
    (are [result arg] (= result (value-of-1 (brightness-level arg)))
      1 1
      1 nil
      2 2)))

(deftest contrast-level-test
  (testing "read or change the contrast-level"
    (are [result arg] (= result (value-of-1 (contrast-level arg)))
      64 64
      64 nil
      63 63)))

(deftest sharpness-mode-test
  (testing "reading and altering sharpness mode"
    (are [result arg] (= result (value-of (night-mode arg)))
      1 true
      0 false
      0 nil)))

(deftest exposure-mode-test
  (testing "reading and altering exposure mode"
    (are [result arg] (= result (value-of (exposure-mode arg)))
      1 true
      0 false
      0 nil)))

(deftest gain-mode-test
  (testing "reading and altering gain mode"
    (are [result arg] (= result (value-of (gain-mode arg)))
      1 true
      0 false
      0 nil)))

(deftest mirror-test
  (testing "reading and altering mirror setting"
    (are [result arg] (= result (value-of (mirror arg)))
      1 true
      0 false
      0 nil)))

(deftest flip-test
  (testing "reading and altering flip setting"
    (are [result arg] (= result (value-of (flip arg)))
      1 true
      0 false
      0 nil)))

(deftest de-noise-mode-test
  (testing "reading and altering de-noise mode"
    (are [result arg] (= result (value-of (de-noise-mode arg)))
      1 true
      0 false
      0 nil)))(deftest de-noise-mode-test

(testing "reading and altering banding filter mode"
    (are [result arg] (= result (value-of (banding-filter-mode arg)))
      1 true
      0 false
      0 nil))


(deftest soft-reset-test
  (testing "soft reset"
    (let [_ (soft-reset!)]
      (is (= 1 (value-of (exposure-mode))))
      (is (= 1 (value-of (gain-mode))))))))
