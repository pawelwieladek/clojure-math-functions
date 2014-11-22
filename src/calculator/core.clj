(ns calculator.core
  (:gen-class))

(defn abs
  [x]
  (if (> x 0) x (- x)))

(defn power
  [x y]
  (if (> y 0) (* x (power x (dec y))) 1))

(defn round
  [d precision]
  (let [factor (power 10 precision)]
    (double (/ (int (* d factor)) factor))))

(defn sqrt
  [x]
  (let [eps 10e-15, a x]
    (letfn [(newton [x a eps] (if (<= (abs (- (* a a) x)) eps) (double a) (newton x (double (/ (+ x (* a a)) (* 2 a))) eps)))]
      (newton x a eps))))

(defn factorial
  [x]
  (if (> x 0) (* x (factorial (dec x))) 1))

(defn sin
  [x]
  (def sum 0)
  (def n 1)
  (loop [i 1]
    (when (< i 10e3)
      (def n (* n (/ x i)))
      (cond
        (= (mod i 4) 1) (def sum (+ sum n))
        (= (mod i 4) 3) (def sum (- sum n)))
      (recur (inc i))))
  (double sum))

(defn cos
  [x]
  (def sum 1)
  (def n 1)
  (loop [i 1]
    (when (< i 10e3)
      (def n (* n (/ x i)))
      (cond
        (= (mod i 4) 0) (def sum (+ sum n))
        (= (mod i 4) 2) (def sum (- sum n)))
      (recur (inc i))))
  (double sum))

(defn exp
  [x]
  (def sum 1)
  (def n 1)
  (loop [i 1]
    (when (< i 10e3)
      (def n (* n (double (/ x i))))
      (def sum (+ sum n))
      (recur (inc i))))
  (double sum))

(defn ln
  [y]
  (def precision 10e2)
  (letfn [(log [i x xpowered]
                      (if (<= i precision) (+ (double (/ (* xpowered x) i)) (log (inc i) x (* xpowered x))) 0))]
              (- (log 1 (- 1 y) 1))))

(defn binomial
  [a n]
  (if (>= n 1) (* (/ (+ (- a n) 1) n) (binomial a (dec n))) 1))

(defn -main
  "Clojure Calculator"
  [& args]
  (println "Clojure Calculator. See more in tests."))
