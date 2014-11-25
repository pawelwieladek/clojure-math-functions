(ns calculator.functions
  (:gen-class)
  (:require [calculator.helpers :refer :all]))

; Recursive functions

(defn factorial
  "Factorial of value x"
  [x]
  (if (> x 0) (* x (factorial (dec x))) 1))

(defn power
  "Power of x to y"
  [x y]
  (if (> y 0) (* x (power x (dec y))) 1))

; Square root
; http://en.wikipedia.org/wiki/Methods_of_computing_square_roots
(defn sqrt
  "Square root of value x"
  [x & [precision]]
  (let [precision (if (nil? precision) PRECISION precision)]
    (let [eps precision, a x]
      (letfn [(newton [x a eps] (if (<= (Math/abs (- (* a a) x)) eps) (double a) (newton x (double (/ (+ x (* a a)) (* 2 a))) eps)))]
        (newton x a eps)))))

(defn binomial
  "Binomial of values a and n"
  [a n]
  (if (>= n 1) (* (/ (+ (- a n) 1) n) (binomial a (dec n))) 1))

(defn exp
  "Expontent od value x"
  [x & [limit]]
  (let [limit (if (nil? limit) STACK_SIZE limit)]
    ((fn $ [sum n i]
       (if (<= i limit) (+ sum ($ n (* n (double (/ x i))) (inc i))) 0)) 0 1 1)))

(defn ln
  "Natural logarithm of value x from a range (0, 1)"
  [a & [limit]]
  (let [limit (if (nil? limit) STACK_SIZE limit)]
    (let [precision STACK_SIZE]
      (letfn [($ [i x y] (if (<= i precision) (+ (double (/ (* y x) i)) ($ (inc i) x (* y x))) 0))]
        (- ($ 1 (- 1 a) 1))))))

(defn sin
  "Sine of value x"
  [x & [limit]]
  (let [limit (if (nil? limit) STACK_SIZE limit)
        init_sum 0
        init_nth 1
        init_iteration 1]
    ((fn $ [sum_element nth_element iteration]
       (if (<= iteration limit)
         (let [nth_element (* nth_element (double (/ x iteration)))]
           (let [sum_element (cond
                               (= (mod iteration 4) 1) nth_element
                               (= (mod iteration 4) 3) (- nth_element)
                               :else 0)]
            (+ sum_element ($ sum_element nth_element (inc iteration)))))
         0))
      init_sum init_nth init_iteration)))

(defn sinh
  "Hyperbolic sine of value x"
  [x & [limit]]
  (let [limit (if (nil? limit) STACK_SIZE limit)
        init_sum 0
        init_nth 1
        init_iteration 1]
    ((fn $ [sum_element nth_element iteration]
       (if (<= iteration limit)
         (let [nth_element (* nth_element (double (/ x iteration)))]
           (let [sum_element (if (or (= (mod iteration 4) 1) (= (mod iteration 4) 3)) nth_element 0)]
             (+ sum_element ($ sum_element nth_element (inc iteration)))))
         0))
      init_sum init_nth init_iteration)))

(defn cos
  "Cosine of value x"
  [x & [limit]]
  (let [limit (if (nil? limit) STACK_SIZE limit)
        init_sum 1
        init_nth 1
        init_iteration 0]
    ((fn $ [sum_element nth_element iteration]
       (if (<= iteration limit)
         (let [nth_element (cond (= iteration 0) 1 :else (* nth_element (double (/ x iteration))))]
           (let [sum_element (cond
                               (= (mod iteration 4) 0) nth_element
                               (= (mod iteration 4) 2) (- nth_element)
                               :else 0)]
             (+ sum_element ($ sum_element nth_element (inc iteration)))))
         0))
      init_sum init_nth init_iteration)))

(defn cosh
  "Hyperbolic cosine of value x"
  [x & [limit]]
  (let [limit (if (nil? limit) STACK_SIZE limit)
        init_sum 1
        init_nth 1
        init_iteration 0]
    ((fn $ [sum_element nth_element iteration]
       (if (<= iteration limit)
         (let [nth_element (cond (= iteration 0) 1 :else (* nth_element (double (/ x iteration))))]
           (let [sum_element (if (or (= (mod iteration 4) 0) (= (mod iteration 4) 2)) nth_element 0)]
             (+ sum_element ($ sum_element nth_element (inc iteration)))))
         0))
      init_sum init_nth init_iteration)))
