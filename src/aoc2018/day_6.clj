(ns aoc2018.day-6
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn parse [input]
  "multiline string input 을 정수 sequence 로 변경해 return 하는 function"
  (->> input
       clojure.string/split-lines
       (map
         (fn [string]
           (let [[x y] (map #(Integer/parseInt %) (next (re-find (re-matcher #"(\d+), (\d+)" string))))]
             [x y])))))
;; ([137 140] [318 75] [205 290] [104 141] [163 104] [169 164] [238 324] [180 166] [260 198] [189 139] [290 49] [51 350] [51 299] [73 324] [220 171] [146 336] [167 286] [51 254] [40 135] [103 138] [100 271] [104 328] [80 67] [199 180] [320 262] [215 290] [96 142] [314 128] [162 106] [214 326] [303 267] [340 96] [211 278] [335 250] [41 194] [229 291] [45 97] [304 208] [198 214] [250 80] [200 51] [287 50] [120 234] [106 311] [41 116] [359 152] [189 207] [300 167] [318 315] [296 72])


(def input (->> "aoc2018/day_6_input"
                io/resource
                slurp
                parse))

;(def input (->> "1, 1\n1, 6\n8, 3\n3, 4\n5, 5\n8, 9"
;                parse))

(defn calculate-inclination
  "기울기를 계산하는 function"
  [[px py] [x y]]
  (/ (- x px) (- y py)))

(defn ccw?
  "a, b point 를 기준으로 c 의 방향이 반시계 방향인지 확인하는 function"
  [[ax ay] [bx by] [cx cy]]
  (pos? (- (* (- bx ax) (- cy ay)) (* (- cx ax) (- by ay)))))

(defn find-convex-hull-points
  "points 중 가장 자리의 points 들을 찾아 return 하는 function"
  [points]
  (->> (let [sorted (sort-by (juxt second first) points)
             first-point (first sorted)
             others (sort-by #(calculate-inclination first-point %) (next sorted))]
         (reduce (fn [points current-point]
                   (if (apply ccw? (concat (take-last 2 points) [current-point]))
                     (conj (pop points) current-point)
                     (conj points current-point)))
                 [first-point (first others)]
                 (next others)))))

(defn find-points-in-ranges
  "points 를 포함한 padding 만큼의 영역에 속한 모든 points 를 return 하는 function"
  [points padding]
  (let [sorted-by-x (sort-by first points)
        sorted-by-y (sort-by second points)]
    (for [x (range (- (first (first sorted-by-x)) padding) (+ (+ padding 1) (first (last sorted-by-x))))
          y (range (- (second (first sorted-by-y)) padding) (+ (+ padding 1) (second (last sorted-by-y))))]
      [x y])))

(defn manhattan-distance
  "(x,y) 를 기준으로 points 들의 manhattan distance 를 계산해 map 으로 return 하는 function"
  [[x y] points]
  (->> points
       (reduce (fn [distances [px py]]
                (assoc distances [px py] (+ (Math/abs (- py y)) (Math/abs (- px x)))))
               {})))

(defn closest-point
  "거리가 계산된 points map 를 확인하여 가장 가까운 point 를 return 하는 function"
  [points]
  (cond
    (= 0 (count points)) nil
    (= 1 (count points)) (first (first points))
    :else (let [[[p1 d1] [_ d2]] (sort-by val points)]
            (when (< d1 d2)
              p1))))

(defn solve-part-1 [input]
  (let [edge-point-set (set (find-convex-hull-points input))
        all-points-in-range (find-points-in-ranges edge-point-set 0)]
    (->> all-points-in-range
         (reduce (fn [distances x]
                   (assoc distances x (manhattan-distance x input)))
                 {})
         (map #(closest-point (val %)))
         (filter #(and (nil? (edge-point-set %)) (not-empty %)))
         frequencies
         (sort-by val)
         last
         last)))

(comment
  (solve-part-1 input))

(defn solve-part-2 [input max]
  (let [edge-point-set (set (find-convex-hull-points input))
        all-points-in-range (find-points-in-ranges edge-point-set max)]
    (->> all-points-in-range
         (map #(manhattan-distance % input))
         (map #(vals %))
         (map #(apply + %))
         (filter #(< % max))
         count)))

(comment
  (solve-part-1 input 10000))