(ns calculator.core
  (:gen-class)
  (:require [calculator.helpers :refer :all]
            [calculator.functions :refer :all]
            [calculator.macros :refer :all]))

(defn -main
  "Clojure Calculator"
  [& args]
  (println "Clojure Calculator"))