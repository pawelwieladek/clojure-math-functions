(ns calculator.helpers-test
  (:require [clojure.test :refer :all]
            [calculator.helpers :refer :all]
            [calculator.common :refer :all]))

(deftest abs-test
  (testing "Absolute value"
    (is (= (abs 0) 0))
    (is (= (abs 2) 2))
    (is (= (abs -9) 9))))

(deftest round-test
  (testing "Round"
    (is (= (round 0.1357 3) 0.135))
    (is (= (round 0.1357 2) 0.13))
    (is (= (round 0.1357 1) 0.1))
    (is (= (round 0.1357 0) 0.0))))
