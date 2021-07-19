(ns aoc2018.day-6
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse [input]
  "multiline string input 을 정수 sequence 로 변경해 return 하는 function"
  (->> input
       clojure.string/split-lines
       (map
         (fn [string]
           (let [[x y] (map #(Integer/parseInt %) (next (re-find (re-matcher #"(\d+), (\d+)" string))))]
             [x y])))))
;; ([137 140] [318 75] [205 290] [104 141] [163 104] [169 164] [238 324] [180 166] [260 198] [189 139] [290 49] [51 350] [51 299] [73 324] [220 171] [146 336] [167 286] [51 254] [40 135] [103 138] [100 271] [104 328] [80 67] [199 180] [320 262] [215 290] [96 142] [314 128] [162 106] [214 326] [303 267] [340 96] [211 278] [335 250] [41 194] [229 291] [45 97] [304 208] [198 214] [250 80] [200 51] [287 50] [120 234] [106 311] [41 116] [359 152] [189 207] [300 167] [318 315] [296 72])


;(def input (->> "aoc2018/day_6_input"
;                io/resource
;                slurp
;                parse))

(def input (->> "1, 1\n1, 6\n8, 3\n3, 4\n5, 5\n8, 9"
                parse))

(defn calculate-inclination [[px py] [x y]]
  (/ (- x px) (- y py)))

(defn ccw? [[ax ay] [bx by] [cx cy]]
  (pos? (- (* (- bx ax) (- cy ay)) (* (- cx ax) (- by ay)))))

(defn find-convex-hull-points [input]
  (->> (let [sorted (sort-by (juxt second first) input)
             first-point (first sorted)
             others (sort-by #(calculate-inclination first-point %) (next sorted))]
         (reduce (fn [points current-point]
                   (if (apply ccw? (concat (take-last 2 points) [current-point]))
                     (conj (pop points) current-point)
                     (conj points current-point)))
                 [first-point (first others)]
                 (next others)))))

(find-convex-hull-points input)