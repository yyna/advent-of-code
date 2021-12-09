(ns _2021.day_7
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn parse [path]
  (map #(Integer/parseInt %) (-> path
                                 io/resource
                                 slurp
                                 (s/split #"\,"))))

(defn gauss-sum
  [n]
  (/ (* n (inc n)) 2))

(comment

  (def crabs-position (parse "2021/day_7_input"))

  ;;[part 1] How much fuel must they spend to align to that position?
  (let [min-value (apply min crabs-position)
        max-value (apply max crabs-position)]
    (apply min (for [number (range min-value (inc max-value))]
                 (apply + (for [position crabs-position]
                            (Math/abs (- number position)))))))

  ;;[part 2] How much fuel must they spend to align to that position?
  (let [min-value (apply min crabs-position)
        max-value (apply max crabs-position)]
      (apply min (for [number (range min-value (inc max-value))]
                   (apply + (for [position crabs-position]
                              (gauss-sum (Math/abs (- number position))))))))

  ,)
