(ns calculator.helpers
  (:gen-class))

; Helpers
(defn abs
  [x]
  (if (> x 0) x (- x)))

(defn round
  [d precision]
  (let [factor (Math/pow 10 precision)]
    (double (/ (int (* d factor)) factor))))
