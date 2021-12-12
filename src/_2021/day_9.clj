(ns _2021.day_9
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.set :as set]))

(defn parse [path]
  (let [lines (-> path
                  io/resource
                  slurp
                  s/split-lines)]
    (->> lines
         (map #(s/split % #""))
         (mapv (fn [v]
                 (mapv #(Integer/parseInt %) v))))))

(defn lowest-point?
  [heightmap x y]
  (let [value (get-in heightmap [x y])
        up (get-in heightmap [(dec x) y])
        down (get-in heightmap [(inc x) y])
        left (get-in heightmap [x (dec y)])
        right (get-in heightmap [x (inc y)])]
    (and (or (nil? up) (< value up))
         (or (nil? down) (< value down))
         (or (nil? left) (< value left))
         (or (nil? right) (< value right)))))

(defn risk-level
  [heightmap x y]
  (inc (get-in heightmap [x y])))

(defn basin
  ([heightmap x y]
   (basin heightmap x y #{}))
  ([heightmap x y point-set]
   (let [value (get-in heightmap [x y])]
     (if-not (or (nil? value) (= 9 value))
       (let [current-point-set (conj point-set [x y])]
         (-> current-point-set
             (set/union (when-not (contains? current-point-set [(dec x) y])
                          (basin heightmap (dec x) y current-point-set)))
             (set/union (when-not (contains? current-point-set [(inc x) y])
                          (basin heightmap (inc x) y current-point-set)))
             (set/union (when-not (contains? current-point-set [x (dec y)])
                          (basin heightmap x (dec y) current-point-set)))
             (set/union (when-not (contains? current-point-set [x (inc y)])
                          (basin heightmap x (inc y) current-point-set)))))
       point-set))))

(comment
  (def heightmap (parse "2021/day_9_input"))

  ;;[part 1] What is the sum of the risk levels of all low points on your heightmap?
  (->> (for [x (range (count heightmap))
             y (range (count (first heightmap)))]
         [heightmap x y])
       (filter #(apply lowest-point? %))
       (map #(apply risk-level %))
       (apply +))

  ;;[part 2] What do you get if you multiply together the sizes of the three largest basins?
  (->> (for [x (range (count heightmap))
             y (range (count (first heightmap)))]
         [heightmap x y])
       (filter #(apply lowest-point? %))
       (map #(apply basin %))
       (map count)
       (sort >)
       (take 3)
       (apply *))

  ,)
