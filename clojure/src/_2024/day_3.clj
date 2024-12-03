(ns _2024.day-3
  (:require [clojure.java.io :as io]))

(defn parse
  [path]
  (->> path
       (io/resource)
       (slurp)))

(defn mul [s]
  (let [[_ a b] (re-find #"mul\((\d+),(\d+)\)" s)]
    (* (parse-long a) (parse-long b))))

(comment
  @(def input (parse "2024/day_3_input"))

  ;; [part 1] What do you get if you add up all of the results of the multiplications?
  (->> input
       (re-seq #"mul\(\d+,\d+\)")
       (map mul)
       (apply +))

  ;; [part 2] what do you get if you add up all of the results of just the enabled multiplications?
  (->> input
       (re-seq #"don't\(\)|do\(\)|mul\(\d+,\d+\)")
       (reduce (fn [{:keys [x sum]} instruction]
                 (case instruction
                   "don't()" {:x 0 :sum sum}
                   "do()" {:x 1 :sum sum}
                   {:x x :sum (+ sum (* x (mul instruction)))}))
               {:x 1 :sum 0})
       :sum)

  :rcf)
