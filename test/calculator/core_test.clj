(ns calculator.core-test
  (:require [clojure.test :refer :all]
            [calculator.core :refer :all]))

(deftest abs-test
  (testing "Absolute value"
    (is (= (abs 0) 0))
    (is (= (abs 2) 2))
    (is (= (abs -9) 9))))

(deftest round-test
  (testing "Round"
    (is (= (round 0.1234 3) 0.123))
    (is (= (round 0.1234 2) 0.12))
    (is (= (round 0.1234 1) 0.1))
    (is (= (round 0.1234 0) 0.0))))

(deftest power-test
  (testing "Power"
    (is (= (power 2 0) 1))
    (is (= (power 0 2) 0))
    (is (= (power 2 2) 4))
    (is (= (power 5 3) 125))))

(deftest sqrt-test
  (testing "Square root"
    (is (= (sqrt 1) 1.0))
    (is (= (sqrt 2) 1.414213562373095))
    (is (= (sqrt 9) 3.0))))

(deftest factorial-test
  (testing "Factorial"
    (is (= (factorial 0) 1))
    (is (= (factorial 2) 2))
    (is (= (factorial 5) 120))))

(deftest sin-test
  (testing "Sine"
    (is (= (sin (/ Math/PI 3)) 0.8660254037844385))))

(deftest cos-test
  (testing "Cosine"
    (is (= (cos (/ Math/PI 6)) 0.8660254037844386))))

(deftest exp-test
  (testing "Exponent"
    (is (= (exp 0) 1.0))
    (is (= (exp 1) 2.7182818284590455))
    (is (= (exp 2) 7.3890560989306495))))

(deftest binomial-test
  (testing "Binomial"
    (is (= (binomial 5 2) 10))
    (is (= (binomial 3 2) 3))
    (is (= (binomial 2 2) 1))))

(deftest ln-test
  (testing "Ln from 0 to 1"
    (is (= (ln 0.2) -1.6094379124341007))
    (is (= (ln 0.5) -0.6931471805599453))))
