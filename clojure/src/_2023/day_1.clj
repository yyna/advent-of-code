(ns _2023.day-1
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn parse
  [path]
  (-> path
      (io/resource)
      (slurp)
      (string/split-lines)))

(defn ->two-digit-number [s]
  (let [x (re-seq #"\d" s)]
    (parse-long (str (first x) (last x)))))

(defn first-digit [s]
  (let [found (re-find #"1|2|3|4|5|6|7|8|9|one|two|three|four|five|six|seven|eight|nine" s)]
    (case found
      "one" "1"
      "two" "2"
      "three" "3"
      "four" "4"
      "five" "5"
      "six" "6"
      "seven" "7"
      "eight" "8"
      "nine" "9"
      found)))

(defn last-digit [s]
  (let [found (re-find #"1|2|3|4|5|6|7|8|9|eno|owt|eerht|ruof|evif|xis|neves|thgie|enin" (string/reverse s))]
    (case found
      "eno" "1"
      "owt" "2"
      "eerht" "3"
      "ruof" "4"
      "evif" "5"
      "xis" "6"
      "neves" "7"
      "thgie" "8"
      "enin" "9"
      found)))

(defn ->calibration-value [s]
  (->> (str (first-digit s) (last-digit s))
       (parse-long)))

(comment
  @(def input (parse "2023/day_1_input"))

  ;;[part 1] What is the sum of all of the calibration values?
  (->> input
       (map ->two-digit-number)
       (apply +))

  ;;[part 2] What is the sum of all of the calibration values?
  (->> input
       (map ->calibration-value)
       (apply +))

  :rcf)
