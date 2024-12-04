(ns _2024.day-3
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn parse
  [path]
  (->> path
       (io/resource)
       (slurp)
       (string/split-lines)
       (map #(into [] %))
       (into [])))

(defn horizontal
  [array x y length]
  (let [width (count array)]
    (->> (subvec (get array x) y (min (+ y length) width))
         (apply str))))

(defn horizontal-backward
  [array x y length]
  (->> (subvec (get array x) (max (inc (- y length)) 0) (inc y))
       (reverse)
       (apply str)))

(defn vertical
  [array x y length]
  (->> (for [x-index (range x (+ x length))]
         (get-in array [x-index y] nil))
       (apply str)))
(defn vertical-backward
  [array x y length]
  (->> (for [x-index (range (inc (- x length)) (inc x))]
         (get-in array [x-index y] nil))
       (reverse)
       (apply str)))

(defn diagonal-bottom-right
  [array x y length]
  (->> (for [i (range 0 length)]
         (get-in array [(+ x i) (+ y i)] nil))
       (apply str)))

(defn diagonal-top-left
  [array x y length]
  (->> (for [i (range 0 length)]
         (get-in array [(- x i) (- y i)] nil))
       (apply str)))

(defn diagonal-top-right
  [array x y length]
  (->> (for [i (range 0 length)]
         (get-in array [(- x i) (+ y i)] nil))
       (apply str)))

(defn diagonal-bottom-left
  [array x y length]
  (->> (for [i (range 0 length)]
         (get-in array [(+ x i) (- y i)] nil))
       (apply str)))

(defn all-coordinates
  [array]
  (for [x (range 0 (count array))
        y (range 0 (count (get array 0)))]
    [x y]))

(defn neighbor-words
  [array x y max-length]
  ((juxt horizontal
         horizontal-backward
         vertical
         vertical-backward
         diagonal-bottom-left
         diagonal-bottom-right
         diagonal-top-left
         diagonal-top-right)
   array x y max-length))

(defn mas-or-sam?
  [word]
  (contains? #{"MAS" "SAM"} word))

(comment
  @(def input (parse "2024/day_4_input"))

  ;; [part 1] How many times does 'XMAS' appear?
  (->> (all-coordinates input)
       (filter (fn [[x y]]
                 (= \X (get-in input [x y]))))
       (map (fn [[x y]]
              (neighbor-words input x y 4)))
       (flatten)
       (filter #(= "XMAS" %))
       (count))

  ;; [part 2] How many times does an X-MAS appear?
  (->> (all-coordinates input)
       (filter (fn [[x y]]
                 (mas-or-sam? (diagonal-bottom-right input x y 3))))
       (filter (fn [[x y]]
                 (mas-or-sam? (diagonal-bottom-left input x (+ 2 y) 3))))
       (count))

  :rcf)
