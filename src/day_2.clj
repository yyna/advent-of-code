(ns day_2)

(defn parse [input]
  "multiline string input 을 string sequence 로 변경해 return 하는 function"
  (clojure.string/split-lines input))

;read input
(def input (->> (slurp "./src/day_2_input")
                (parse)))

;; parse 된 자료 형태를 코멘트로 남기자

;[part 1]
;주어진 각각의 문자열에서, 같은 문자가 두번 혹은 세번씩 나타난다면 각각을 한번씩 센다. 두번 나타난 문자가 있는 문자열의 수 _ 세번 나타난 문자가 있는 문자열의 수를 반환하시오. 예) abcdef 어떤 문자도 두번 혹은 세번 나타나지 않음 -> (두번 나오는 문자열 수: 0, 세번 나오는 문자열 수: 0) bababc 2개의 a, 3개의 b -> (두번 나오는 문자열 수: 1, 세번 나오는 문자열 수: 1) abbcde 2개의 b -> (두번 나오는 문자열 수: 2, 세번 나오는 문자열 수: 1) abcccd 3개의 c -> (두번 나오는 문자열 수: 2, 세번 나오는 문자열 수: 2) aabcdd 2개의 a, 2개의 d 이지만, 한 문자열에서 같은 갯수는 한번만 카운트함 -> (두번 나오는 문자열 수: 3, 세번 나오는 문자열 수: 2) abcdee 2개의 e -> (두번 나오는 문자열 수: 4, 세번 나오는 문자열 수: 2) ababab 3개의 a, 3개의 b 지만 한 문자열에서 같은 갯수는 한번만 카운트함 -> (두번 나오는 문자열 수: 4, 세번 나오는 문자열 수: 3) 답 : 4 _ 3 = 12
(comment
  ;; 전체가 parse?
  (def count_map (->> input
                      (map frequencies)
                      (map vals)
                      (map))) ;some 으로 바꾸고싶다

  ; juxt 사용해보기
  ; {:"}
  (*
    (count (filter #(% 2) count_map))
    (count (filter #(% 3) count_map))))




;[part 2]
;여러개의 문자열 중, 같은 위치에 정확히 하나의 문자가 다른 문자열 쌍에서 같은 부분만을 리턴하시오.

(defn get_common_letters [a b]
  "string a, b 를 input 으로 받아 공통된 문자열을 return 하는 function"
  (->> (map vector a b)
       (reduce (fn [s [x y]] (if (= x y) (str s x) s)) "")))

(defn print_diff [list]
  "string sequence 를 input 으로 받아 모든 조합 중 1개의 알파벳만 다른 경우의 공통된 문자열을 print 하는 function"
  (when (seq list)
    (doseq [b (rest list) :let [a (first list) common_letters (get_common_letters a b)]]
      (when (= (- (count a) 1) (count common_letters))
        (println common_letters)))
    (recur (rest list))))

(comment
    (print_diff input))
