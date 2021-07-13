(ns aoc2020.day_8 (:require [clojure.java.io :as io]))

(defn parse [input]
  "multiline string input 을 정수 sequence 로 변경해 return 하는 function"
  (->> (clojure.string/split-lines input)
       (map #(Integer/parseInt %))))

(def input (->> "aoc2020/day_8_input"
                io/resource
                slurp
                parse))


;; loop recur reduce 없이