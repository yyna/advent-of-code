(ns day_1)

(defn parse [input]
  "multiline string input 을 정수 sequence 로 변경해 return 하는 function"
  (->> (clojure.string/split-lines input)
       (map #(Integer/parseInt %))))

(defn sum [data]
  "number sequence input 의 값을 모두 더한 값을 return 하는 function"
  (apply + data))

;[part 1]
;주어진 입력의 모든 숫자를 더하시오.
;
;예) +10 -2 -5 +1 이 (입력일 경우 4를 출력
(comment
  ;; slurp -> IO
  ;; IO 는 부수효과(side-effect)라고 합니다.
  (->> (slurp "./src/day_1_input")
       (parse)
       (sum)))

(defn find_the_first_duplicate
  "숫자 sequence 를 input 으로 받아 누적 합의 값이 처음으로 두번 나오는 숫자를 return 하는 function"
  ([list] (find_the_first_duplicate list 0 #{}))
  ([list sum count_set]
   (let [current_value (first list) current_sum (+ current_value sum)]
     (if (count_set current_sum)
       current_sum
       (recur (rest list) current_sum (conj count_set current_sum))))))

;[part 2]
;주어진 입력의 숫자를 더할 때 마다 나오는 숫자 중, 처음으로 두번 나오는 숫자를 리턴하시오.
;
;예) +3, +3, +4, -2, -4 는 10이 처음으로 두번 나오는 숫자임. 0 -> 3 (+3) -> 6 (+3) -> 10(+4) -> 8(-2) -> 4(-4) -> 7(+3) -> 10(+3) -> ...
(comment
  (->> (slurp "./src/day_1_input")
       (parse)
       (cycle)
       (find_the_first_duplicate)))
