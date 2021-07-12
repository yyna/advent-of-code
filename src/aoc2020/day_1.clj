(ns aoc2020.day_1 (:require [clojure.java.io :as io]))

(defn parse [input]
  "multiline string input 을 정수 sequence 로 변경해 return 하는 function"
  (->> (clojure.string/split-lines input)
       (map #(Integer/parseInt %))))

(def input (->> "aoc2020/day_1_input"
                io/resource
                slurp
                parse))

(defn find-two-numbers [input n]
  (->> (for [x (range (count input))
             y (range (count input))
             :when (< x y)]
         (let [a (nth input x)
               b (nth input y)]
           {:a a :b b :sum (+ a b)}))
       (filter #(= n (% :sum)))
       first))

;;[part 1]
;;더해서 2020이 되는 두 숫자의 곱을 구하시오. (두 숫자는 유일하다고 가정)
;;
;;예) 1721 979 366 299 675 1456 의 경우 1721 * 299 = 514579 를 출력
(defn solve-part-1 [input]
  (->> (find-two-numbers input 2020)
       ((juxt :a :b))
       (apply *)))

(comment
  (solve-part-1 input))

;;[part 2]
;;같은 입력이 주어질 때, 더해서 2020이 되는 세 숫자의 합을 구하시오.
;;
;;예) 2020 = 979 + 366 + 675, 곱하면 241861950 을 출력