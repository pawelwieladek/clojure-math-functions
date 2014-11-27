(ns calculator.core
  (:gen-class)
  (:require [calculator.helpers :refer :all]
            [calculator.functions :refer :all]
            [calculator.macros :refer :all])
  (:use [clojure.walk]))

(defn -main
  "Clojure Calculator"
  [& args]
  (println (factorial-macro (+ 2 5)))
  (println (macroexpand-all `(factorial-macro (+ 2 5))))
  (println "Clojure Calculator"))