(ns calculator.functions-test
  (:require [clojure.test :refer :all]
            [calculator.helpers :refer :all]
            [calculator.functions :refer :all]
            [calculator.common :refer :all]))

(deftest power-test
  (testing "Power"
    (is (= (power 2 0) 1))
    (is (= (power 0 2) 0))
    (is (= (power 2 2) 4))
    (is (= (power 5 3) 125))
    (is (= (power (+ 1 1) (* 2 3)) 64))))

(deftest sqrt-test
  (testing "Square root"
    (is (= (sqrt 1) 1.0))
    (is (= (sqrt 9) 3.0))
    (is (= (sqrt (* 7 7)) 7.0))
    (test-rounded (sqrt 2) (Math/sqrt 2))))

(deftest factorial-test
  (testing "Factorial"
    (is (= (factorial 0) 1))
    (is (= (factorial 2) 2))
    (is (= (factorial (+ 1 2)) 6))
    (is (= (factorial 5) 120))))

(deftest exp-test
  (testing "Exponent"
    (test-rounded (exp 0) (Math/exp 0))
    (test-rounded (exp 1) (Math/exp 1))
    (test-rounded (exp 2) (Math/exp 2))))

(deftest binomial-test
  (testing "Binomial"
    (is (= (binomial 5 2) 10))
    (is (= (binomial 3 2) 3))
    (is (= (binomial (- 5 2) (+ 1 1)) 3))
    (is (= (binomial 2 2) 1))))

(deftest ln-test
  (testing "Ln from 0 to 1"
    (test-rounded (ln 0.2) (Math/log 0.2))
    (test-rounded (ln 0.5) (Math/log 0.5))))

(deftest sin-test
  (testing "Sine"
    (def angle (/ Math/PI 6))
    (test-rounded (sin angle) (Math/sin angle))
    (def angle (/ Math/PI 4))
    (test-rounded (sin angle) (Math/sin angle))
    (def angle (/ Math/PI 2))
    (test-rounded (sin angle) (Math/sin angle))))

(deftest sinh-test
  (testing "Sine"
    (def angle (/ Math/PI 6))
    (test-rounded (sinh angle) (Math/sinh angle))
    (def angle (/ Math/PI 4))
    (test-rounded (sinh angle) (Math/sinh angle))
    (def angle (/ Math/PI 2))
    (test-rounded (sinh angle) (Math/sinh angle))))

(deftest cos-test
  (testing "Cosine"
    (def angle (/ Math/PI 6))
    (test-rounded (cos angle) (Math/cos angle))
    (def angle (/ Math/PI 4))
    (test-rounded (cos angle) (Math/cos angle))
    (def angle (/ Math/PI 2))
    (test-rounded (cos angle) (Math/cos angle))))

(deftest cosh-test
  (testing "Cosine"
    (def angle (/ Math/PI 6))
    (test-rounded (cosh angle) (Math/cosh angle))
    (def angle (/ Math/PI 4))
    (test-rounded (cosh angle) (Math/cosh angle))
    (def angle (/ Math/PI 2))
    (test-rounded (cosh angle) (Math/cosh angle))))
