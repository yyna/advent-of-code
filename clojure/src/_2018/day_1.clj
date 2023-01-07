(ns _2018.day_1 (:require [clojure.java.io :as io]))

(defn parse
  "multiline string input 을 정수 sequence 로 변경해 return 하는 function"
  [input]
  (->> (clojure.string/split-lines input)
       (map #(Integer/parseInt %))))

(def input (->> "2018/day_1_input"
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

;; loop 로 part 2 풀기
(loop [input (cycle input)
       sum 0
       sum-set #{}]
  (let [sum (+ sum (first input))]
    (if (sum-set sum)
      (println sum)
      (recur (next input) sum (conj sum-set sum)))))

;; reduced 로 part 2 풀기
(reduce
  (fn [{:keys [sum sum-set]} x]
    (let [sum (+ sum x)]
      (if (sum-set sum)
        (reduced sum)
        {:sum sum :sum-set (conj sum-set sum)})))
  {:sum 0 :sum-set #{}} (cycle input))

;; iterate 로 part 2 풀기
(defn add
  [{:keys [input sum sum-set]}]
  (let [sum (+ sum (first input))]
    {:input (next input)
     :sum sum
     :sum-set (conj sum-set sum)
     :no-duplicate? (nil? (sum-set sum))}))

(->> {:input (cycle input) :sum 0 :sum-set #{} :no-duplicate? true}
     (iterate add)
     (drop-while :no-duplicate?)
     first
     :sum)

;; reductions 로 part 2 풀기
(defn first-duplicate
  [input]
  (reduce (fn [set x]
            (if (set x)
              (reduced x)
              (conj set x)))
          #{} input))

;;[part 2]
;;주어진 입력의 숫자를 더할 때 마다 나오는 숫자 중, 처음으로 두번 나오는 숫자를 리턴하시오.
;;
;;예) +3, +3, +4, -2, -4 는 10이 처음으로 두번 나오는 숫자임. 0 -> 3 (+3) -> 6 (+3) -> 10(+4) -> 8(-2) -> 4(-4) -> 7(+3) -> 10(+3) -> ...
(defn solve-part-2 [input]
  (first-duplicate (reductions + (cycle input))))

(comment
  (solve-part-2 input))
