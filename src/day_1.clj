(ns day_1 (:require [clojure.java.io :as io]))

(defn parse [input]
  "multiline string input 을 정수 sequence 로 변경해 return 하는 function"
  (->> (clojure.string/split-lines input)
       (map #(Integer/parseInt %))))

(io/resource "day_1_input")

(slurp (io/resource "day_1_input"))

(defn sum [data]
  "number sequence input 의 값을 모두 더한 값을 return 하는 function"
  (apply + data))

;;[part 1]
;;주어진 입력의 모든 숫자를 더하시오.
;;
;;예) +10 -2 -5 +1 이 (입력일 경우 4를 출력
(defn solve-part-1 [input]
  (->> input
       parse
       sum))
(comment
  ;; slurp -> IO
  ;; IO 는 부수효과(side-effect)라고 합니다.
  (solve-part-1 input))

;;
(defn find-the-first-duplicate
  "숫자 sequence 를 input 으로 받아 누적 합의 값이 처음으로 두번 나오는 숫자를 return 하는 function"
  ([list] (find-the-first-duplicate list 0 #{}))
  ([list sum count-set]
   (let [current-value (first list)
         current-sum (+ current-value sum)]
     (if (count-set current-sum)
       current-sum
       (recur (rest list) current-sum (conj count-set current-sum))))))

;(1) loop - 초기값
;(2) reduce 로 바꿔보기 (reduced 로 중간에 탈출하기)

;slurp 상대경로 X

;;[part 2]
;;주어진 입력의 숫자를 더할 때 마다 나오는 숫자 중, 처음으로 두번 나오는 숫자를 리턴하시오.
;;
;;예) +3, +3, +4, -2, -4 는 10이 처음으로 두번 나오는 숫자임. 0 -> 3 (+3) -> 6 (+3) -> 10(+4) -> 8(-2) -> 4(-4) -> 7(+3) -> 10(+3) -> ...
(comment
  (->> input
       parse
       cycle
       find-the-first-duplicate))
