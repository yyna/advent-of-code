(ns _2022.day-4
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn parse-pair
  [str]
  (->> (re-seq #"\d+" str)
       (map parse-long)
       (partition 2)))

(defn parse
  [path]
  (->> path
       (io/resource)
       (slurp)
       (s/split-lines)
       (map parse-pair)))

(defn fully-contains?
  [[[a b] [c d]]]
  (or (<= a c d b)
      (<= c a b d)))

(defn overlapped?
  [[[a b] [c d]]]
  (or (fully-contains? [[a b] [c d]])
      (<= c b d)
      (<= c a d)))

(comment

  @(def input (parse "2022/day_4_input"))

  ;; [part 1] In how many assignment pairs does one range fully contain the other?
  (->> (map fully-contains? input)
       (filter true?)
       (count))

  ;; [part 2]
  (->> (map overlapped? input)
       (filter true?)
       (count))

  :rcf)
