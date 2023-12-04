(ns _2023.day-2
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn parse
  [path]
  (-> path
      (io/resource)
      (slurp)
      (string/split-lines)))

(defn parse-set [s]
  (->> (re-seq #"(\d+) (blue|red|green)" s)
       (map (fn [[_ cnt color]]
              (hash-map (keyword color) (parse-long cnt))))
       (apply conj)))

(defn parse-game-info [s]
  (let [[_ id & sets] (string/split s #"(Game |:|;)")]
    {:id (parse-long id)
     :sets (map parse-set sets)}))

(defn possible-set? [game-info]
  (let [{:keys [red green blue]
         :or {red 0 green 0 blue 0}} game-info]
    (and (<= red 12)
         (<= green 13)
         (<= blue 14))))

(comment
  @(def input (parse "2023/day_2_input"))

  ;;[part 1] What is the sum of the IDs of those games?
  (->> input
       (map parse-game-info)
       (filter #(every? possible-set? (:sets %)))
       (map :id)
       (apply +))

  ;;[part 2] What is the sum of the power of these sets?
  (->> input
       (map parse-game-info)
       (map #(apply merge-with max (:sets %)))
       (map vals)
       (map #(apply * %))
       (apply +))

  :rcf)
