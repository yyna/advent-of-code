(ns day_1)

(defn parse [input]
  "input, output, 무슨 일 하는지"
  (->> (clojure.string/split-lines input)
       (map #(Integer/parseInt %))))

(defn sum
  "input, output, 무슨 일 하는지"
  [data]
  (apply + data))

;[part 1]
;주어진 입력의 모든 숫자를 더하시오.
;
;예) +10 -2 -5 +1 이 (입력일 경우 4를 출력
(comment
  ;; slurp -> IO
  ;; IO 는 부수효과(side-effect)라고 합니다.
  (->> (parse (slurp "./src/day_1_input"))
       (sum)))


;[part 2]
;주어진 입력의 숫자를 더할 때 마다 나오는 숫자 중, 처음으로 두번 나오는 숫자를 리턴하시오.
;
;예) +3, +3, +4, -2, -4 는 10이 처음으로 두번 나오는 숫자임. 0 -> 3 (+3) -> 6 (+3) -> 10(+4) -> 8(-2) -> 4(-4) -> 7(+3) -> 10(+3) -> ...
(defn find_the_first_duplicate
  ([list] (find_the_first_duplicate list 0 #{}))
  ([list sum count_set]
   (let [current_value (first list) current_sum (+ current_value sum)]
     (if (count_set current_sum)
       current_sum
       (recur (rest list) current_sum (conj count_set current_sum))))))

(comment
  (->> (parse (slurp "./src/day_1_input"))
       (cycle)
       (find_the_first_duplicate)))
