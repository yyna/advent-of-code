(ns _2021.day_1
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn parse [path]
  (->> path
       io/resource
       slurp
       s/split-lines
       (map #(Integer/parseInt %))))

(defn count-increase [input]
  (->> input
       (partition 2 1)
       (filter #(apply < %))
       count))

(comment

  (def input (parse "2021/day_1_input"))

  ;;[part 1] How many measurements are larger than the previous measurement?
  (count-increase input)

  ;;[part 2] Consider sums of a three-measurement sliding window. How many sums are larger than the previous sum?
  (->> input
       (partition 3 1)
       (map #(apply + %))
       count-increase)

  ,)


