(ns aoc2018.day_1 (:require [clojure.java.io :as io]))

(defn parse [input]
  "multiline string input 을 정수 sequence 로 변경해 return 하는 function"
  (->> (clojure.string/split-lines input)
       (map #(Integer/parseInt %))))

(def input (->> "aoc2018/day_1_input"
                io/resource
                slurp
                parse))
;; (1 2 3 4)

;;[part 1]
;;주어진 입력의 모든 숫자를 더하시오.
;;
;;예) +10 -2 -5 +1 이 입력일 경우 4를 출력
(defn solve-part-1 [input]
  (->> input
       (apply +)))

(comment
  (solve-part-1 input))

(defn add
  [{:keys [list sum current-set]}]
  (let [current-sum (+ sum (first list))]
    {:list (next list)
     :sum current-sum
     :current-set (conj current-set current-sum)
     :duplicated? (contains? current-set current-sum)}))

;;[part 2]
;;주어진 입력의 숫자를 더할 때 마다 나오는 숫자 중, 처음으로 두번 나오는 숫자를 리턴하시오.
;;
;;예) +3, +3, +4, -2, -4 는 10이 처음으로 두번 나오는 숫자임. 0 -> 3 (+3) -> 6 (+3) -> 10(+4) -> 8(-2) -> 4(-4) -> 7(+3) -> 10(+3) -> ...
(defn solve-part-2 [input]
  (->> {:list (cycle input) :sum 0 :current-set #{} :duplicated? false}
       (iterate add)
       (drop-while #(= false (:duplicated? %)))
       first
       :sum))

(comment
  (solve-part-2 input))
