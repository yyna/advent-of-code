(ns _2023.day-3
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn parse
  [path]
  (-> path
      (io/resource)
      (slurp)
      (string/split-lines)))

(defn engine-symbol?
  [s]
  (contains? #{"@" "#" "$" "%" "&" "*" "/" "." "+" "=" "-"} s))

(defn number|symbol
  [s]
  (-> (if (engine-symbol? s)
        {:symbol s}
        {:number (parse-long s)})
      (assoc :length (count s))))

(defn parse-line
  [s]
  (->> (re-seq #"\d+|[@#$%&*/+.=-]" s)
       (map number|symbol)))

(defn add-x-position
  [xs]
  (reduce (fn [xs a]
            (let [x (if-let [last-element (last xs)]
                      (+ (:x last-element) (:length last-element))
                      0)]
              (conj xs (assoc a :x x))))
          [] xs))

(defn add-y-position
  [xs]
  (reduce (fn [xs a]
            (let [y (if-let [last-element (last xs)]
                      (inc (:y (first last-element)))
                      0)]
              (conj xs (map #(assoc % :y y) a))))
          [] xs))

(defn adjacent-to-symbol?
  [number symbols]
  (let [{:keys [x y length]} number]
    (->> (for [i (range (dec x) (+ x length 1))
               j (range (dec y) (+ y 2))
               :when (not (and (= y j)
                               (< (dec x) i (+ x length))))]
           [i j])
         (map #(get symbols [(first %) (second %)]))
         (remove nil?)
         (empty?)
         (not))))

(defn gear-ratio
  [numbers gear]
  (let [adjacent-numbers (filter #(adjacent-to-symbol? % {gear "*"}) numbers)]
    (if (= 2 (count adjacent-numbers))
      (->> adjacent-numbers
           (map :number)
           (apply *))
      0)))

(comment
  (def input (->> (parse "2023/day_3_input")
                  (map parse-line)
                  (map add-x-position)
                  (add-y-position)
                  (flatten)
                  (remove #(= (:symbol %) "."))))

  (def numbers (filter :number input))
  ;; ({:number 467, :length 3, :x 0, :y 0}
  ;;  {:number 114, :length 3, :x 5, :y 0}
  ;;  {:number 35, :length 2, :x 2, :y 2}
  ;;  {:number 633, :length 3, :x 6, :y 2}
  ;;  {:number 617, :length 3, :x 0, :y 4}
  ;;  {:number 58, :length 2, :x 7, :y 5}
  ;;  {:number 592, :length 3, :x 2, :y 6}
  ;;  {:number 755, :length 3, :x 6, :y 7}
  ;;  {:number 664, :length 3, :x 1, :y 9}
  ;;  {:number 598, :length 3, :x 5, :y 9})

  (def symbols (->> input
                    (filter :symbol)
                    (reduce (fn [xs a]
                              (let [{:keys [x y symbol]} a]
                                (assoc xs [x y] symbol)))
                            {})))
  ;; {[3 1] "*", [6 3] "#", [3 4] "*", [5 5] "+", [3 8] "$", [5 8] "*"}

  ;;[part 1] What is the sum of all of the part numbers in the engine schematic?
  (->> numbers
       (filter #(adjacent-to-symbol? % symbols))
       (map :number)
       (apply +))

  ;;[part 2] What is the sum of all of the gear ratios in your engine schematic?
  @(def gears (->> symbols
                   (filter #(= "*" (val %)))
                   (keys)))
  ;; ([3 1] [3 4] [5 8])

  (->> gears
       (map #(gear-ratio numbers %))
       (apply +))

  :rcf)
