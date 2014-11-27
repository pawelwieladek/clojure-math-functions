(ns calculator.macros
  (:gen-class)
  (:require [calculator.helpers :refer :all]))

(defmacro power-macro
  "Power of x to y"
  [x y]
  (if (> (eval y) 0) `(* ~x (power-macro ~x (dec ~y))) 1))

(defmacro factorial-macro
  "Factorial of value x"
  [x]
  (if (> (eval x) 1) `(* ~x (factorial-macro (dec ~x))) 1))

(defmacro binomial-macro [a n]
  "Binomial of values a and n"
  (if (>= (eval n) 1) `(* (/ (+ (- ~a ~n) 1) ~n) (binomial-macro ~a (dec ~n))) 1))

(defmacro sqrt-macro
  "Square root of value x"
  [x & [precision]]
  (let [precision (if (nil? precision) 10e-15 precision)]
    `((fn !# [x# a# eps#] (if (<= (Math/abs (- (* a# a#) x#)) eps#)
                            (double a#)
                            (!# x# (double (/ (+ x# (* a# a#)) (* 2 a#))) eps#))) ~x ~x ~precision)))

(defmacro ln-macro
  "Natural logarithm of value x from a range (0, 1)"
  [a & [precision]]
  (let [precision (if (nil? precision) 10e2 precision)]
    `(- ((fn !# [i# x# y#] (if (<= i# ~precision) (+ (double (/ (* y# x#) i#)) (!# (inc i#) x# (* y# x#))) 0)) 1 (- 1 ~a) 1))))

(defmacro exp-macro
  "Expontent od value x"
  [a & [precision]]
  (let [precision (if (nil? precision) 10e2 precision)]
    `((fn !# [sum# n# i#] (if (<= i# ~precision) (+ sum# (!# n# (* n# (double (/ ~a i#))) (inc i#))) 0)) 0 1 1)))

(defmacro sin-macro
  "Sine of value x"
  [x & [limit]]
  (let [limit (if (nil? limit) STACK_SIZE limit)
        init_sum 0
        init_nth 1
        init_iteration 1]
    `((fn $# [sum_element# nth_element# iteration#]
        (if (<= iteration# ~limit)
          (let [nth_element# (* nth_element# (double (/ ~x iteration#)))]
            (let [sum_element# (cond
                         (= (mod iteration# 4) 1) nth_element#
                         (= (mod iteration# 4) 3) (- nth_element#)
                         :else 0)]
              (+ sum_element# ($# sum_element# nth_element# (inc iteration#)))))
          0))
       ~init_sum ~init_nth ~init_iteration)))

(defmacro cos-macro
  "Cosine of value x"
  [x & [limit]]
  (let [limit (if (nil? limit) STACK_SIZE limit)
        init_sum 1
        init_nth 1
        init_iteration 0]
    `((fn $# [sum_element# nth_element# iteration#]
       (if (<= iteration# ~limit)
         (let [nth_element# (cond (= iteration# 0) 1 :else (* nth_element# (double (/ ~x iteration#))))]
           (let [sum_element# (cond
                               (= (mod iteration# 4) 0) nth_element#
                               (= (mod iteration# 4) 2) (- nth_element#)
                               :else 0)]
             (+ sum_element# ($# sum_element# nth_element# (inc iteration#)))))
         0))
      ~init_sum ~init_nth ~init_iteration)))

(defmacro sinh-macro
  "Hyperbolic sine of value x"
  [x & [limit]]
  (let [limit (if (nil? limit) STACK_SIZE limit)
        init_sum 0
        init_nth 1
        init_iteration 1]
    `((fn $# [sum_element# nth_element# iteration#]
       (if (<= iteration# ~limit)
         (let [nth_element# (* nth_element# (double (/ ~x iteration#)))]
           (let [sum_element# (if (or (= (mod iteration# 4) 1) (= (mod iteration# 4) 3)) nth_element# 0)]
             (+ sum_element# ($# sum_element# nth_element# (inc iteration#)))))
         0))
      ~init_sum ~init_nth ~init_iteration)))

(defmacro cosh-macro
  "Hyperbolic cosine of value x"
  [x & [limit]]
  (let [limit (if (nil? limit) STACK_SIZE limit)
        init_sum 1
        init_nth 1
        init_iteration 0]
    `((fn $# [sum_element# nth_element# iteration#]
       (if (<= iteration# ~limit)
         (let [nth_element# (cond (= iteration# 0) 1 :else (* nth_element# (double (/ ~x iteration#))))]
           (let [sum_element# (if (or (= (mod iteration# 4) 0) (= (mod iteration# 4) 2)) nth_element# 0)]
             (+ sum_element# ($# sum_element# nth_element# (inc iteration#)))))
         0))
      ~init_sum ~init_nth ~init_iteration)))
