(ns aoc2018.day_4 (:require [clojure.java.io :as io]))
(import java.text.SimpleDateFormat)
;; java-time
;; java 없이 해보기

(defn generate-sleep-await-map [[asleep-map last-guard-id] x]
  ""
  (let [[datetime guard-id] (rest (re-find (re-matcher #"\[(\d+-\d+-\d+ \d+:\d+)\] (?:Guard #(\d+) )?(?:begins shift|falls asleep|wakes up)" x)))]
    (if (nil? guard-id)
      [(assoc asleep-map last-guard-id (conj (asleep-map last-guard-id) datetime)) last-guard-id]
      (let [number-guard-id (Integer/parseInt guard-id)]
        [(assoc asleep-map number-guard-id (into [] (asleep-map number-guard-id))) number-guard-id]))))

(defn parse [input]
  "multiline string input 을 string sequence 로 변경해 return 하는 function"
  (->> input
       clojure.string/split-lines
       sort
       (reduce generate-sleep-await-map [{} 0])
       first))

(def input (->> "aoc2018/day_4_input"
                io/resource
                slurp
                parse))
;;{10 [1518-11-01 00:05 1518-11-01 00:25 1518-11-01 00:25 1518-11-01 00:30 1518-11-01 00:55]
;; 99 [1518-11-02 00:40 1518-11-02 00:50] ...}

(defn get-most-frequent [input]
  (->> input
       frequencies
       (sort-by val)
       last))

;; doto
;; ..
(defn get-asleep-mins [[a b]]
  "date a ~ date b 사이의 모든 minute 리스트를 return 하는 function
  (2021-07-09 00:15 2021-07-09 00:21) -> (15 16 17 18 19 20)"
  (let
    [start-min (Integer/parseInt (nth (clojure.string/split a #":") 1))
     diff (/
            (-
              (.getTime (.parse (SimpleDateFormat. "yyyy-MM-dd HH:mm") b))
              (.getTime (.parse (SimpleDateFormat. "yyyy-MM-dd HH:mm") a)))
            60000)]
    (map #(mod % 60) (range start-min (+ start-min diff)))))

(defn generate-sleep-min-map [minute-map [guard-id sleeps]]
  (->> (partition 2 sleeps)
       (map get-asleep-mins)
       (apply concat)
       (assoc minute-map guard-id)))

(defn get-longer-map [max [k v]]
  (if (or (empty? max) (> (count v) (count (:val max))))
    {:key k :val v}
    max))

;;[part 1]
;;키워드: 가드(Guard) 번호, 자는 시간(falls asleep), 일어나는 시간(wakes up). 각 가드들은 교대 근무를 시작하고 (begins shift) 졸았다가 일어났다를 반복함. 위의 예시에서 10번 가드는 0시 5분에 잤다가 25분에 일어나고, 또 0시 30분에 잠들었다가 0시 55분에 깨어남. 가드들에 대해서 자고 깨는 시간 정보들이 입력으로 주어짐. 파트 1은 “주어진 입력에 대해서, 가장 오랜시간 잠들어있었던 가드의 ID와, 그 가드가 가장 빈번하게 잠들어 있었던 분(minute)의 곱을 구하라” 만약 20번 가드가 0시 10분36분, 2시 5분11분, 3시 11분~13분 이렇게 잠들어 있었다면, “11분“이 가장 빈번하게 잠들어 있던 ‘분’. 그럼 답은 20 * 11 = 220.
(defn solve-part-1 [input]
  (->> input
       (reduce generate-sleep-min-map {})
       (reduce get-longer-map {})
       ((juxt :key #(nth (get-most-frequent (:val %)) 0)))
       (apply *)))

(comment
  (solve-part-1 input))

;;[part 2]
;;주어진 분(minute)에 가장 많이 잠들어 있던 가드의 ID과 그 분(minute)을 곱한 값을 구하라.
(defn solve-part-2 [input]
  (->> input
       (reduce generate-sleep-min-map {})
       (filter (fn [[_ asleep-mins]] (> (count asleep-mins) 0)))
       (map (fn [[guard-id asleep-mins]]
              (let [[minute frequency] (get-most-frequent asleep-mins)]
                {:guard-id guard-id :minute minute :frequency frequency})))
       (sort-by :frequency)
       last
       ((juxt :guard-id :minute))
       (apply *)))

(comment
  (solve-part-2 input))