(ns clj-arduino.hx711-test
  (:require [clojure.test :refer :all]
            [clj-arduino.hx711 :refer :all]))



(deftest push-test
  (testing "pushing messages onto the queue"
    (dosync (alter queue empty))
    (is (= 1 (do (push! 2020202020) (count @queue))))))
