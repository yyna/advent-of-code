(ns aoc2018.day_3 (:require [clojure.java.io :as io]))

(defn parse [input]
  "multiline string input 을 string sequence 로 변경해 return 하는 function"
  (->> input
       clojure.string/split-lines
       (map
         (fn [string]
           (let [[id x y w h] (map #(Integer/parseInt %) (rest (re-find (re-matcher #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)" string))))]
             {:id id :coordinates [x y] :size [w h]})))))
;; ({:id 1 :coordinates [1 3] :size [4 4]}
;;  {:id 2 :coordinates [3 1] :size [4 4]}
;;  {:id 3 :coordinates [5 5] :size [2 2]})

(def input (->> "aoc2018/day_3_input"
                io/resource
                slurp
                parse))

(defn generate [{id :id [x y] :coordinates [w h] :size}]
  "(x,y) 좌표의 너비 w 높이 h 인 면적에 속하는 좌표의 list 를 return 하는 function
  {:id 1 :coordinates [1 3] :size [4 4]} -> ([1 1 3] [1 1 4] [1 1 5] [1 1 6] [1 2 3] [1 2 4] [1 2 5] [1 2 6] [1 3 3] [1 3 4] [1 3 5] [1 3 6] [1 4 3] [1 4 4] [1 4 5] [1 4 6])"
  (for [a (range x (+ x w))
        b (range y (+ y h))]
    [id a b])) ;; map 으로 바꿔보자

(defn map-ids-to-coordinates [list]
  "각 좌표에 나타나는 영역의 id 배열을 map 으로 return 하는 function
  ([1 1 1] [1 2 1] [2 1 1]) -> {[1 1] (1 2) [1 2] (1)}"
  (reduce
    (fn [m [id x y]]
      (assoc m [x y] (conj (m [x y]) id)))
    {} list))

;;[part 1]
;;다음과 같은 입력이 주어짐.
;;
;;#1 @ 1,3: 4x4
;;#2 @ 3,1: 4x4
;;#3 @ 5,5: 2x2
;;# 뒤에 오는 숫자는 ID, @ 뒤에 오는 숫자 쌍 (a, b)는 시작 좌표, : 뒤에 오는 (c x d)는 격자를 나타냄. 입력의 정보대로 격자 공간을 채우면 아래와 같이 됨.
;;
;;........
;;...2222.
;;...2222.
;;.11XX22.
;;.11XX22.
;;.111133.
;;.111133.
;;........
;;여기서 XX는 ID 1, 2, 3의 영역이 두번 이상 겹치는 지역. 겹치는 지역의 갯수를 출력하시오. (위의 예시에서는 4)
(defn solve-part-1 [input]
  (->> input
       (map generate)
       (apply concat)
       map-ids-to-coordinates
       vals
       (filter #(> (count %)  1))
       count))

(comment
  (solve-part-1 input))

;;[part 2]
;;입력대로 모든 격자를 채우고 나면, 정확히 한 ID에 해당하는 영역이 다른 어떤 영역과도 겹치지 않음 위의 예시에서는 ID 3 이 ID 1, 2와 겹치지 않음. 3을 출력. 겹치지 않는 영역을 가진 ID를 출력하시오. (문제에서 답이 하나만 나옴을 보장함)
(defn solve-part-2 [input]
  (->> input
       (map generate)
       (apply concat)
       map-ids-to-coordinates ; parse
       vals
       (reduce (fn [[set duplicate-set] x] ; 함수로 빼자
                 [(into set x)
                  (into duplicate-set (when (> (count x) 1) x))])
               [#{} #{}])
       (apply clojure.set/difference)
       first))

(comment
  (solve-part-2 input))
