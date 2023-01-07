(ns _2021.day_13
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.set :as set]))

(defn parse [path]
  (let [input (-> path
                  io/resource
                  slurp)]
    {:dots (->> (re-seq #"\d+,\d+" input)
                (map #(s/split % #"\,"))
                (map (fn [[x y]]
                       {:x (Integer/parseInt x)
                        :y (Integer/parseInt y)}))
                set)
     :instructions (map (fn [[_ axis num]]
                          {:axis (keyword axis)
                           :num (Integer/parseInt num)})
                        (re-seq #"fold along (x|y)=(\d+)" input))}))

(defn move
  [dots axis num]
  (if (= :y axis)
    (reduce (fn [acc {:keys [x y]}]
              (conj acc {:x x :y (- y (* 2 (- y num)))}))
            #{} dots)
    (reduce (fn [acc {:keys [x y]}]
              (conj acc {:x (- x (* 2 (- x num))) :y y}))
            #{} dots)))

(defn fold
  [dots {:keys [axis num]}]
  (let [stopping-dots (filter #(< (axis %) num) dots)
        moving-dots (filter #(> (axis %) num) dots)
        dots-after-move (move moving-dots axis num)]
    (set/union (set stopping-dots) dots-after-move)))

(defn draw-dots
  [dots]
  (let [max-x (apply max (map :x dots))
        max-y (apply max (map :y dots))]
    (->> (for [y (range (inc max-y))
               x (range (inc max-x))]
           {:x x :y y})
         (map (fn [drawing-dot]
                (if (contains? dots drawing-dot)
                  (print "■")
                  (print "□"))
                (when (= (:x drawing-dot) max-x)
                  (println))))
         count)))

(comment
  (def dots-and-instructions (parse "2021/day_13_input"))

  ;; [part 1] How many dots are visible after completing just the first fold instruction on your transparent paper?
  (-> (:dots dots-and-instructions)
      (fold (-> dots-and-instructions :instructions first))
      count)

  ;; [part 2] What code do you use to activate the infrared thermal imaging camera system?
  (->> (reduce (fn [dots instruction]
                 (fold dots instruction))
               (:dots dots-and-instructions)
               (:instructions dots-and-instructions))
       draw-dots)

  ,)