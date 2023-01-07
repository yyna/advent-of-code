(ns _2022.day-2
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn parse
  [path]
  (->> path
       io/resource
       slurp
       s/split-lines))

(defn opponent->shape
  [character]
  (case character
    "A" :rock
    "B" :paper
    "C" :scissor))

(defn ->strategy
  [line guide]
  (let [[opponent me] (s/split line #" ")]
    {:opponent (opponent->shape opponent)
     :me       (get guide [opponent me])}))

(defn shape->score
  [shape]
  (case shape
    :rock 1
    :paper 2
    :scissor 3))

(defn calculate-score
  [{:keys [opponent me]}]
  (cond
    ;; draw
    (= opponent me)
    {:opponent (+ 3 (shape->score opponent))
     :me       (+ 3 (shape->score me))}

    ;; opponent wins
    (or (and (= opponent :rock)
             (= me :scissor))
        (and (= opponent :paper)
             (= me :rock))
        (and (= opponent :scissor)
             (= me :paper)))
    {:opponent (+ 6 (shape->score opponent))
     :me       (shape->score me)}

    ;; i win
    :else
    {:opponent (shape->score opponent)
     :me       (+ 6 (shape->score me))}))

(comment

  (def input (parse "2022/day_2_input"))

  ;;[part 1] What would your total score be if everything goes exactly according to your strategy guide?
  (->> input
       (map #(->strategy % {["A" "X"] :rock
                            ["A" "Y"] :paper
                            ["A" "Z"] :scissor
                            ["B" "X"] :rock
                            ["B" "Y"] :paper
                            ["B" "Z"] :scissor
                            ["C" "X"] :rock
                            ["C" "Y"] :paper
                            ["C" "Z"] :scissor}))
       (map calculate-score)
       (reduce (fn [{:keys [opponent me]} acc]
                 (-> acc
                     (update :opponent + opponent)
                     (update :me + me)))
               {:opponent 0
                :me       0}))

  ;;[part 2] what would your total score be if everything goes exactly according to your strategy guide?
  (->> input
       (map #(->strategy % {["A" "X"] :scissor
                            ["A" "Y"] :rock
                            ["A" "Z"] :paper
                            ["B" "X"] :rock
                            ["B" "Y"] :paper
                            ["B" "Z"] :scissor
                            ["C" "X"] :paper
                            ["C" "Y"] :scissor
                            ["C" "Z"] :rock}))
       (map calculate-score)
       (reduce (fn [{:keys [opponent me]} acc]
                 (-> acc
                     (update :opponent + opponent)
                     (update :me + me)))
               {:opponent 0
                :me       0}))

  :rcf)

