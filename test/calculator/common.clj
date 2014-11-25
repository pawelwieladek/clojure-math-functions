(ns calculator.common
  (:require [calculator.helpers :refer :all]
            [clojure.test :refer :all]))

(def ROUND-PRECISION 3)

(defmacro test-rounded [expected actual]
  `(is (= (round ~expected ROUND-PRECISION) (round ~actual ROUND-PRECISION))))