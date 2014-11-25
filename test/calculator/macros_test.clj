(ns calculator.macros-test
  (:require [clojure.test :refer :all]
            [calculator.helpers :refer :all]
            [calculator.macros :refer :all]
            [calculator.common :refer :all]))

(deftest power-macro-test
  (testing "Power macro"
    (is (= (power-macro 2 0) 1))
    (is (= (power-macro 0 2) 0))
    (is (= (power-macro 2 2) 4))
    (is (= (power-macro 5 3) 125))))

(deftest sqrt-macro-test
  (testing "Square root macro"
    (is (= (sqrt-macro 1) 1.0))
    (is (= (sqrt-macro 9) 3.0))
    (test-rounded (sqrt-macro 2) (Math/sqrt 2))))

(deftest factorial-macro-test
  (testing "Factorial macro"
    (is (= (factorial-macro 0) 1))
    (is (= (factorial-macro 2) 2))
    (is (= (factorial-macro 5) 120))))

(deftest exp-macro-test
  (testing "Exponent macro"
    (test-rounded (exp-macro 0) (Math/exp 0))
    (test-rounded (exp-macro 1) (Math/exp 1))
    (test-rounded (exp-macro 2) (Math/exp 2))))

(deftest binomial-macro-test
  (testing "Binomial macro"
    (is (= (binomial-macro 5 2) 10))
    (is (= (binomial-macro 3 2) 3))
    (is (= (binomial-macro 2 2) 1))))

(deftest ln-macro-test
  (testing "Ln macro from 0 to 1"
    (test-rounded (ln-macro 0.2) (Math/log 0.2))
    (test-rounded (ln-macro 0.5) (Math/log 0.5))))

(deftest sin-macro-test
  (testing "Sine macro"
    (def angle (/ Math/PI 6))
    (test-rounded (sin-macro angle) (Math/sin angle))
    (def angle (/ Math/PI 4))
    (test-rounded (sin-macro angle) (Math/sin angle))
    (def angle (/ Math/PI 2))
    (test-rounded (sin-macro angle) (Math/sin angle))))

(deftest sinh-macro-test
  (testing "Sine macro"
    (def angle (/ Math/PI 6))
    (test-rounded (sinh-macro angle) (Math/sinh angle))
    (def angle (/ Math/PI 4))
    (test-rounded (sinh-macro angle) (Math/sinh angle))
    (def angle (/ Math/PI 2))
    (test-rounded (sinh-macro angle) (Math/sinh angle))))

(deftest cos-macro-test
  (testing "Cosine macro"
    (def angle (/ Math/PI 6))
    (test-rounded (cos-macro angle) (Math/cos angle))
    (def angle (/ Math/PI 4))
    (test-rounded (cos-macro angle) (Math/cos angle))
    (def angle (/ Math/PI 2))
    (test-rounded (cos-macro angle) (Math/cos angle))))

(deftest cosh-macro-test
  (testing "Cosine macro"
    (def angle (/ Math/PI 6))
    (test-rounded (cosh-macro angle) (Math/cosh angle))
    (def angle (/ Math/PI 4))
    (test-rounded (cosh-macro angle) (Math/cosh angle))
    (def angle (/ Math/PI 2))
    (test-rounded (cosh-macro angle) (Math/cosh angle))))