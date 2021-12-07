(ns _2021.day_5
  (:require [clojure.java.io :as io]))

(defn parse [path]
  (->> path
       io/resource
       slurp
       s/split-lines
       (map (fn [line]
              (let [[x1 y1 x2 y2] (->> (re-seq #"\d+" line)
                                       (map #(Integer/parseInt %)))]
                {:x1 x1 :y1 y1 :x2 x2 :y2 y2})))))

(defn my-range
  [x y]
  (if (<= x y)
    (range x (inc y))
    (reverse (range y (inc x)))))

(defn line->coordinates
  [{:keys [x1 y1 x2 y2]}]
  (cond
    (= x1 x2) (for [y (my-range (min y1 y2) (max y1 y2))]
                [x1 y])
    (= y1 y2) (for [x (my-range (min x1 x2) (max x1 x2))]
                [x y1])
    :else (map vector (my-range x1 x2) (my-range y1 y2))))

(defn diagonal?
  [{:keys [x1 y1 x2 y2]}]
  (and (not= y1 y2)
       (= 1 (Math/abs (/ (- x1 x2) (- y1 y2))))))

(defn count-overlap-points
  [lines]
  (->> lines
       (map line->coordinates)
       (apply concat)
       frequencies
       (filter #(< 1 (val %)))
       count))

(comment

  (def lines (parse "2021/day_5_input"))

  ;;[part 1] Consider only horizontal and vertical lines. At how many points do at least two lines overlap?
  (->> lines
       (filter (fn [{:keys [x1 y1 x2 y2]}]
                 (or (= x1 x2) (= y1 y2))))
       count-overlap-points)

  ;;[part 2] Consider all of the lines. At how many points do at least two lines overlap?
  (->> lines
       (filter (fn [{:keys [x1 y1 x2 y2] :as line}]
                 (or (= x1 x2) (= y1 y2) (diagonal? line))))
       count-overlap-points)

  ,)