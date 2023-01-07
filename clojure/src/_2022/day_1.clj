(ns _2022.day-1
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn parse-line [line]
  (->> (s/split-lines line)
       (map parse-long)))

(defn parse
  [path]
  (map parse-line (-> path
                      io/resource
                      slurp
                      (s/split #"\n\n"))))

(comment

  (def input (parse "2022/day_1_input"))

  ;;[part 1] Find the Elf carrying the most Calories. How many total Calories is that Elf carrying?
  (->> input
       (map #(apply + %))
       (apply max))

  ;;[part 2] Find the top three Elves carrying the most Calories. How many Calories are those Elves carrying in total?
  (->> input
       (map #(apply + %))
       (sort #(compare %2 %1))
       (take 3)
       (apply +))

  :rcf)

