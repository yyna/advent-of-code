(ns aoc2018.day_2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse [input]
  "multiline string input 을 string sequence 로 변경해 return 하는 function"
  (str/split-lines input))

(def input (->> "aoc2018/day_2_input"
                io/resource
                slurp
                parse))
;; [abc def ghi]

;[part 1]
;주어진 각각의 문자열에서, 같은 문자가 두번 혹은 세번씩 나타난다면 각각을 한번씩 센다. 두번 나타난 문자가 있는 문자열의 수 _ 세번 나타난 문자가 있는 문자열의 수를 반환하시오. 예) abcdef 어떤 문자도 두번 혹은 세번 나타나지 않음 -> (두번 나오는 문자열 수: 0, 세번 나오는 문자열 수: 0) bababc 2개의 a, 3개의 b -> (두번 나오는 문자열 수: 1, 세번 나오는 문자열 수: 1) abbcde 2개의 b -> (두번 나오는 문자열 수: 2, 세번 나오는 문자열 수: 1) abcccd 3개의 c -> (두번 나오는 문자열 수: 2, 세번 나오는 문자열 수: 2) aabcdd 2개의 a, 2개의 d 이지만, 한 문자열에서 같은 갯수는 한번만 카운트함 -> (두번 나오는 문자열 수: 3, 세번 나오는 문자열 수: 2) abcdee 2개의 e -> (두번 나오는 문자열 수: 4, 세번 나오는 문자열 수: 2) ababab 3개의 a, 3개의 b 지만 한 문자열에서 같은 갯수는 한번만 카운트함 -> (두번 나오는 문자열 수: 4, 세번 나오는 문자열 수: 3) 답 : 4 _ 3 = 12
(defn count-list-includes [n list]
  "n 을 가지고 있는 list 의 수를 return 하는 function"
  (count (filter #(% n) list)))

(defn solve-part-1 [input]
  (->> input
       (map frequencies)
       (map vals)
       (map frequencies)
       ((juxt #(count-list-includes 2 %) #(count-list-includes 3 %)))
       (apply *)))

(comment
  (last (diff (seq "abcd") (seq "abed")))
  (solve-part-1 input))

;[part 2]
;여러개의 문자열 중, 같은 위치에 정확히 하나의 문자가 다른 문자열 쌍에서 같은 부분만을 리턴하시오.

(defn get-common-letters [a b]
  "string a, b 를 input 으로 받아 공통된 문자열을 return 하는 function
   ex) [abcd, abcf] -> abc"
  (time (->> (map vector a b)
             (reduce (fn [s [x y]] (if (= x y) (str s x) s)) ""))))

  ;⁉️ diff 너무 느림 - 알아보자
  ;(time (->> (clojure.data/diff (seq a) (seq b))
  ;           last
  ;           clojure.string/join)))

(defn get-diff [list]
  "string sequence 를 input 으로 받아 모든 조합 중 1개의 알파벳만 다른 경우의 공통된 문자열을 return 하는 function"
  (->> (for [x (range (count list))
             y (range (count list))
             :when (< x y)]
         (let [a (nth list x)
               b (nth list y)]
           (get-common-letters a b)))
       (filter #(= (+ (count %) 1) (count (first list))))
       first))

(defn solve-part-2 [input]
  (get-diff input))

(comment
  (solve-part-2 input))
