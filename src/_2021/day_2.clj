(ns _2021.day_2
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn parse [path]
  (->> path
       io/resource
       slurp
       s/split-lines
       (map (fn [line]
              (let [[direction distance] (s/split line #"\s")]
                {:direction (keyword direction)
                 :distance (Integer/parseInt distance)})))))

(defn move
  [[horizontal-position depth] {:keys [direction distance]}]
  (case direction
    :forward [(+ horizontal-position distance) depth]
    :up [horizontal-position (- depth distance)]
    :down [horizontal-position (+ depth distance)]))

(defn move-and-aim
  [[horizontal-position depth aim] {:keys [direction distance]}]
  (case direction
    :forward [(+ horizontal-position distance) (+ depth (* aim distance)) aim]
    :up [horizontal-position depth (- aim distance)]
    :down [horizontal-position depth (+ aim distance)]))

(comment

  (def planned-course (parse "2021/day_2_input"))

  ;;[part 1] What do you get if you multiply your final horizontal position by your final depth?
  (->> planned-course
       (reduce #(move %1 %2) [0 0])
       (apply *))

  ;;[part 2] What do you get if you multiply your final horizontal position by your final depth?
  (->> planned-course
       (reduce #(move-and-aim %1 %2) [0 0 0])
       pop
       (apply *))

  ,)

