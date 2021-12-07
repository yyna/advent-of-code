(ns _2021.day_6
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn parse [path]
  (mapv #(Integer/parseInt %) (-> path
                                  io/resource
                                  slurp
                                  (s/split #"\,"))))

(defn ->fish-count
  [timer]
  (reduce #(update %1 %2 inc) [0 0 0 0 0 0 0 0 0] timer))

(defn after-1-day
  [fish-count]
  (let [parent (first fish-count)]
    (-> (into [] (next fish-count))
        (conj parent)
        (update 6 #(+ parent %)))))

(defn total-fish-after-n-day
  [timer n]
  (->> (->fish-count timer)
       (iterate after-1-day)
       (take (inc n))
       last
       (apply +)))

(comment

  (def internal-timer (parse "2021/day_6_input"))

  ;;[part 1] How many lanternfish would there be after 80 days?
  (total-fish-after-n-day internal-timer 80)

  ;;[part 2] How many lanternfish would there be after 256 days?
  (total-fish-after-n-day internal-timer 256)

  ,)

