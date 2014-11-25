(ns calculator.functions-test
  (:require [clojure.test :refer :all]
            [calculator.helpers :refer :all]
            [calculator.functions :refer :all]))

(def P 3)

(defmacro test-rounded [expected actual]
  `(is (= (round ~expected P) (round ~actual P))))

(deftest power-test
  (testing "Power"
    (is (= (power 2 0) 1))
    (is (= (power 0 2) 0))
    (is (= (power 2 2) 4))
    (is (= (power 5 3) 125))))

(deftest sqrt-test
  (testing "Square root"
    (is (= (sqrt 1) 1.0))
    (is (= (sqrt 9) 3.0))
    (test-rounded (sqrt 2) (Math/sqrt 2))))

(deftest factorial-test
  (testing "Factorial"
    (is (= (factorial 0) 1))
    (is (= (factorial 2) 2))
    (is (= (factorial 5) 120))))

(deftest sin-test
  (testing "Sine"
    (def angle (/ Math/PI 6))
    (test-rounded (sin angle) (Math/sin angle))))

(deftest cos-test
  (testing "Cosine"
    (def angle (/ Math/PI 6))
    (test-rounded (cos angle) (Math/cos angle))))

(deftest exp-test
  (testing "Exponent"
    (test-rounded (exp 0) (Math/exp 0))
    (test-rounded (exp 1) (Math/exp 1))
    (test-rounded (exp 2) (Math/exp 2))))

(deftest binomial-test
  (testing "Binomial"
    (is (= (binomial 5 2) 10))
    (is (= (binomial 3 2) 3))
    (is (= (binomial 2 2) 1))))

(deftest ln-test
  (testing "Ln from 0 to 1"
    (test-rounded (ln 0.2) (Math/log 0.2))
    (test-rounded (ln 0.5) (Math/log 0.5))))
