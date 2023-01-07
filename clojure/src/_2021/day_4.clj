(ns _2021.day_4
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn parse-board
  [board-string]
  {:board (->> board-string
               (re-seq #"\d+")
               (mapv (fn [v]
                       {:value (Integer/parseInt v)
                        :marked? false})))
   :bingo false})

(defn parse [path]
  (let [[n & b] (-> path
                 io/resource
                 slurp
                 (s/split #"\n\n"))]
    {:numbers (->> (s/split n #",")
                   (map #(Integer/parseInt %)))
     :boards (map parse-board b)}))

(defn bingo-helper
  [board n1 n2 n3 n4 n5]
  (->> ((juxt #(nth % n1) #(nth % n2) #(nth % n3) #(nth % n4) #(nth % n5)) board)
       (map :marked?)
       (every? true?)))

(defn bingo?
  [board]
  (or (bingo-helper board 0 1 2 3 4)
      (bingo-helper board 5 6 7 8 9)
      (bingo-helper board 10 11 12 13 14)
      (bingo-helper board 15 16 17 18 19)
      (bingo-helper board 20 21 22 23 24)
      (bingo-helper board 0 5 10 15 20)
      (bingo-helper board 1 6 11 16 21)
      (bingo-helper board 2 7 12 17 22)
      (bingo-helper board 3 8 13 18 23)
      (bingo-helper board 4 9 14 19 24)))

(defn mark
  [number {:keys [board]}]
  (let [new-board (->> board
                       (map (fn [{:keys [value] :as b}]
                              (if (= value number)
                                {:value value :marked? true}
                                b))))]
    {:board new-board
     :bingo (bingo? new-board)}))

(defn draw-number
  [{:keys [numbers boards]}]
  (let [number (first numbers)
        next-boards (map #(mark number %) boards)]
    {:numbers (next numbers)
     :boards (remove :bingo next-boards)
     :bingo-board (first (filter :bingo next-boards))
     :last-number number}))

(defn final-score
  [{:keys [bingo-board last-number]}]
  (* last-number (->> (:board bingo-board)
                      (remove :marked?)
                      (map :value)
                      (apply +))))

(comment
  (def input (parse "2021/day_4_input"))

  ;;[part 1] What will your final score be if you choose that board?
  (->> (iterate draw-number input)
       (drop-while #(nil? (:bingo-board %)))
       first
       final-score)

  ;;[part 2] Once it wins, what would its final score be?
  (->> (iterate draw-number input)
       (drop-while #(seq (:boards %)))
       first
       final-score)

  ,)