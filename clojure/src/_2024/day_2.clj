(ns _2024.day-2
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn parse
  [path]
  (->> path
       (io/resource)
       (slurp)
       (string/split-lines)
       (map #(->> (re-seq #"\d+" %)
                  (mapv parse-long)))))

(defn all-pos-or-all-neg?
  [coll]
  (or (every? pos? coll)
      (every? neg? coll)))

(defn all-under-three?
  [coll]
  (every? #(<= % 3) coll))

(defn safe?
  [coll]
  (let [steps (->> (partition 2 1 coll)
                   (map #(- (first %) (second %))))]
    (and (all-pos-or-all-neg? steps)
         (all-under-three? (map abs steps)))))

(defn remove-nth [v n]
  (vec (concat (subvec v 0 n) (subvec v (inc n)))))

(defn safe-if-remove-1?
  [coll]
  (->> (for [n (range 0 (count coll))]
         (remove-nth coll n))
       (some safe?)))

(comment
  @(def input (parse "2024/day_2_input"))

  ;; [part 1] How many reports are safe?
  (->> input
       (filter safe?)
       (count))

  ;; [part 2] Update your analysis by handling situations where the Problem Dampener can remove a single level from unsafe reports. How many reports are now safe?
  (let [result (group-by safe? input)
        safe-count (count (get result true))
        safe-count-if-remove-1  (->> (get result false)
                                     (filter safe-if-remove-1?)
                                     (count))]
    (+ safe-count safe-count-if-remove-1))

  :rcf)
