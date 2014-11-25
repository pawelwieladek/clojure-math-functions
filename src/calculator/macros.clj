(ns calculator.macros
  (:gen-class))

(defmacro power-macro [x y]
  (if (> y 0) `(* ~x (power-macro ~x ~(dec y))) `1))

(defmacro factorial-macro [x]
  (if (> x 1) `(* ~x (factorial-macro ~(dec x))) `1))

(defmacro binomial-macro [a n]
  (if (>= n 1) `(* (/ (+ (- ~a ~n) 1) ~n) (binomial-macro ~a ~(dec n))) `1))

(defmacro sqrt-macro
  [x & [precision]]
  (let [precision (if (nil? precision) 10e-15 precision)]
    `((fn !# [x# a# eps#] (if (<= (Math/abs (- (* a# a#) x#)) eps#)
                            (double a#)
                            (!# x# (double (/ (+ x# (* a# a#)) (* 2 a#))) eps#))) ~x ~x ~precision)))

(defmacro ln-macro [a & [precision]]
  (let [precision (if (nil? precision) 10e2 precision)]
    `(- ((fn !# [i# x# y#] (if (<= i# ~precision) (+ (double (/ (* y# x#) i#)) (!# (inc i#) x# (* y# x#))) 0)) 1 (- 1 ~a) 1))))

(defmacro exp-macro [a & [precision]]
  (let [precision (if (nil? precision) 10e2 precision)]
    `((fn !# [sum# n# i#] (if (<= i# ~precision) (+ sum# (!# n# (* n# (double (/ ~a i#))) (inc i#))) 0)) 0 1 1)))
