(ns aoc2018.day_3 (:require [clojure.java.io :as io]))

(defn generate
  "(x,y) 좌표의 너비 w 높이 h 인 면적에 속하는 좌표의 list 를 return 하는 function
  {:id 1 :coordinates [1 3] :size [4 4]} -> (({:id 1, :x 1, :y 3} {:id 1, :x 1, :y 4} {:id 1, :x 1, :y 5} {:id 1, :x 1, :y 6} {:id 1, :x 2, :y 3} {:id 1, :x 2, :y 4} {:id 1, :x 2, :y 5} {:id 1, :x 2, :y 6} {:id 1, :x 3, :y 3} {:id 1, :x 3, :y 4} {:id 1, :x 3, :y 5} {:id 1, :x 3, :y 6} {:id 1, :x 4, :y 3} {:id 1, :x 4, :y 4} {:id 1, :x 4, :y 5} {:id 1, :x 4, :y 6}))"
  [{id :id [x y] :coordinates [w h] :size}]
  (for [a (range x (+ x w))
        b (range y (+ y h))]
    {:id id
     :x a
     :y b}))

(defn map-ids-to-coordinates
  "각 좌표에 나타나는 영역의 id 배열을 map 으로 return 하는 function
  ({:id 1 :x 1 :y 1} {:id 1 :x 2 :y 1} {:id 2 :x 1 :2 1}) -> {[1 1] (1 2) [1 2] (1)}"
  [list]
  (reduce
    (fn [m {:keys [id x y]}]
      (assoc m [x y] (conj (m [x y]) id)))
    {} list))

(defn parse
  "multiline string input 을 string sequence 로 변경해 return 하는 function"
  [input]
  (->> input
       clojure.string/split-lines
       (map
         (fn [string]
           (let [[id x y w h] (map #(Integer/parseInt %) (rest (re-find (re-matcher #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)" string))))]
             {:id id :coordinates [x y] :size [w h]})))
       (map generate)
       (apply concat)
       map-ids-to-coordinates))
;; {[x y] (id) ...}
;;
;; ex)
;; {[4 3] (2 1)
;;  [2 3] (1)
;;  [2 5] (1)
;;  [3 3] (2 1)
;;  [5 4] (2)
;;  [6 3] (2)
;;  [3 4] (2 1)
;;  ... }

(def input (->> "aoc2018/day_3_input"
                io/resource
                slurp
                parse))

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
       (filter #(> (count (val %)) 1))
       count))

(comment
  (solve-part-1 input))

(defn find-duplicate-set
  "ids-to-coordinates 정보를 받아 [전체 id 의 set, 겹치는 부분이 있는 id 의 set] 을 return 하는 함수"
  [ids-to-coordinates]
  (reduce (fn [[set duplicate-set] x]
            [(into set x)
             (into duplicate-set (when (> (count x) 1) x))])
          [#{} #{}] (vals ids-to-coordinates)))

;;[part 2]
;;입력대로 모든 격자를 채우고 나면, 정확히 한 ID에 해당하는 영역이 다른 어떤 영역과도 겹치지 않음 위의 예시에서는 ID 3 이 ID 1, 2와 겹치지 않음. 3을 출력. 겹치지 않는 영역을 가진 ID를 출력하시오. (문제에서 답이 하나만 나옴을 보장함)
(defn solve-part-2 [input]
  (->> input
       find-duplicate-set
       (apply clojure.set/difference)
       first))

(comment
  (solve-part-2 input))
