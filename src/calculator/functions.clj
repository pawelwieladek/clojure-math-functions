(ns calculator.functions
  (:gen-class))

; Recursive functions

(defn factorial
  [x]
  (if (> x 0) (* x (factorial (dec x))) 1))

(defn power
  [x y]
  (if (> y 0) (* x (power x (dec y))) 1))

; Square root
; http://en.wikipedia.org/wiki/Methods_of_computing_square_roots
(defn sqrt
  [x]
  (let [eps 10e-15, a x]
    (letfn [(newton [x a eps] (if (<= (Math/abs (- (* a a) x)) eps) (double a) (newton x (double (/ (+ x (* a a)) (* 2 a))) eps)))]
      (newton x a eps))))

(defn binomial
  [a n]
  (if (>= n 1) (* (/ (+ (- a n) 1) n) (binomial a (dec n))) 1))

(defn exp [x]
  ((fn $ [sum n i]
     (if (<= i 10e2) (+ sum ($ n (* n (double (/ x i))) (inc i))) 0)) 0 1 1))

(defn ln
  [a]
  (let [precision 10e2]
    (letfn [($ [i x y] (if (<= i precision) (+ (double (/ (* y x) i)) ($ (inc i) x (* y x))) 0))]
      (- ($ 1 (- 1 a) 1)))))

(defn sin [x & [limit]]
  (let [limit (if (nil? limit) 10e2 limit)]
    ((fn $ [sum_element nth_element iteration]
       (if (<= iteration limit)
         (let [nth_element (* nth_element (double (/ x iteration)))]
           (let [sum_element (cond
                               (= (mod iteration 4) 1) nth_element
                               (= (mod iteration 4) 3) (- nth_element)
                               :else 0)]
            (+ sum_element ($ sum_element nth_element (inc iteration)))))
         0))
      0 1 1)))

(defn sinh [x & [limit]]
  (let [limit (if (nil? limit) 10e2 limit)]
    ((fn $ [sum_element nth_element iteration]
       (if (<= iteration limit)
         (let [nth_element (* nth_element (double (/ x iteration)))]
           (let [sum_element (if (or (= (mod iteration 4) 1) (= (mod iteration 4) 3)) nth_element 0)]
             (+ sum_element ($ sum_element nth_element (inc iteration)))))
         0))
      0 1 1)))

(defn cos [x & [limit]]
  (let [limit (if (nil? limit) 10e2 limit)]
    ((fn $ [sum_element nth_element iteration]
       (if (<= iteration limit)
         (let [nth_element (cond (= iteration 0) 1 :else (* nth_element (double (/ x iteration))))]
           (let [sum_element (cond
                               (= (mod iteration 4) 0) nth_element
                               (= (mod iteration 4) 2) (- nth_element)
                               :else 0)]
             (+ sum_element ($ sum_element nth_element (inc iteration)))))
         0))
      1 1 0)))

(defn cosh [x & [limit]]
  (let [limit (if (nil? limit) 10e2 limit)]
    ((fn $ [sum_element nth_element iteration]
       (if (<= iteration limit)
         (let [nth_element (cond (= iteration 0) 1 :else (* nth_element (double (/ x iteration))))]
           (let [sum_element (if (or (= (mod iteration 4) 0) (= (mod iteration 4) 2)) nth_element 0)]
             (+ sum_element ($ sum_element nth_element (inc iteration)))))
         0))
      1 1 0)))
