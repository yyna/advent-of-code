(ns _2021.day_11
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn parse [path]
  (let [lines (-> path
                  io/resource
                  slurp
                  s/split-lines)]
    (->> lines
         (map #(s/split % #""))
         (mapv (fn [v]
                 (mapv #(Integer/parseInt %) v))))))

(defn flashable-position
  [octopuses]
  (loop [positions (for [x (range (count octopuses))
                         y (range (count (first octopuses)))]
                     [x y])]
    (let [position (first positions)]
      (cond
        (nil? position) nil
        (> (get-in octopuses position) 9) position
        :else (recur (rest positions))))))

(defn my-update-in
  [m ks f]
  (if (get-in m ks)
    (update-in m ks f)
    m))

(defn flash
  [octopuses]
  (let [[x y] (flashable-position octopuses)]
    (-> octopuses
        (assoc-in [x y] 0)
        (my-update-in [(dec x) y] #(if (zero? %) 0 (inc %)))
        (my-update-in [(inc x) y] #(if (zero? %) 0 (inc %)))
        (my-update-in [x (dec y)] #(if (zero? %) 0 (inc %)))
        (my-update-in [x (inc y)] #(if (zero? %) 0 (inc %)))
        (my-update-in [(dec x) (dec y)] #(if (zero? %) 0 (inc %)))
        (my-update-in [(dec x) (inc y)] #(if (zero? %) 0 (inc %)))
        (my-update-in [(inc x) (dec y)] #(if (zero? %) 0 (inc %)))
        (my-update-in [(inc x) (inc y)] #(if (zero? %) 0 (inc %))))))

(defn flashable?
  [octopuses]
  (not (nil? (flashable-position octopuses))))

(defn step
  [{:keys [octopuses]}]
  (let [increased (mapv #(mapv inc %) octopuses)]
    (if (flashable? increased)
      (let [xs (take-while flashable? (iterate flash increased))]
        {:octopuses (flash (last xs))
         :flashed-count (count xs)})
      {:octopuses increased
       :flashed-count 0})))

(defn all-flash?
  [{:keys [octopuses]}]
  (->> octopuses
       (apply concat)
       (every? zero?)))

(comment
  (def octopuses (parse "2021/day_11_input"))

  ;;[part 1] How many total flashes are there after 100 steps?
  (->> {:octopuses octopuses}
       (iterate step)
       (take 101)
       (drop 1)
       (map :flashed-count)
       (apply +))

  ;;[part 2] What is the first step during which all octopuses flash?
  (->> {:octopuses octopuses}
       (iterate step)
       (take-while (complement all-flash?))
       count)

  ,)