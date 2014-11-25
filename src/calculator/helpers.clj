(ns calculator.helpers
  (:gen-class))

(def PRECISION 10e-15)
(def STACK_SIZE 10e2)

; Helpers
(defn abs
  [x]
  (if (> x 0) x (- x)))

(defn round
  [d precision]
  (let [factor (Math/pow 10 precision)]
    (double (/ (int (* d factor)) factor))))
