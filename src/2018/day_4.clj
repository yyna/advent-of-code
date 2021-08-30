(ns aoc2018.day_4 (:require [clojure.java.io :as io]))

(defn generate-sleep-await-map [[asleep-map last-guard-id] x]
  ""
  (let [[str-minute str-guard-id] (->> x
                                       (re-matcher #"\[\d+-\d+-\d+ \d+:(\d+)\] (?:Guard #(\d+) )?(?:begins shift|falls asleep|wakes up)")
                                       re-find
                                       next)]
    (if (nil? str-guard-id)
      [(assoc asleep-map last-guard-id (conj (asleep-map last-guard-id) (Integer/parseInt str-minute))) last-guard-id]
      (let [guard-id (Integer/parseInt str-guard-id)]
        [(assoc asleep-map guard-id (into [] (asleep-map guard-id))) guard-id]))))

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

;; {:guard-id [잠든시간 깬시간 잠든시간 깬시간] ...}
;;
;; ex)
;; {:10 [5 25 30 55 24 29 45 55]
;;  :99 [40 50 36 46] ...}

(defn generate-sleep-mins
  ""
  [input]
  (reduce (fn [m [guard-id mins]]
            (assoc m guard-id (->> mins
                                   (partition 2)
                                   (map #(range (first %) (second %)))
                                   (apply concat))))
          {} input))

(defn get-most-sleep-min
  [sleep-mins]
  {:guard-id (key sleep-mins)
   :most-sleep-min (->> sleep-mins
                        val
                        frequencies
                        (sort-by val)
                        last)})

;;[part 1]
;;키워드: 가드(Guard) 번호, 자는 시간(falls asleep), 일어나는 시간(wakes up). 각 가드들은 교대 근무를 시작하고 (begins shift) 졸았다가 일어났다를 반복함. 위의 예시에서 10번 가드는 0시 5분에 잤다가 25분에 일어나고, 또 0시 30분에 잠들었다가 0시 55분에 깨어남. 가드들에 대해서 자고 깨는 시간 정보들이 입력으로 주어짐. 파트 1은 “주어진 입력에 대해서, 가장 오랜시간 잠들어있었던 가드의 ID와, 그 가드가 가장 빈번하게 잠들어 있었던 분(minute)의 곱을 구하라” 만약 20번 가드가 0시 10분36분, 2시 5분11분, 3시 11분~13분 이렇게 잠들어 있었다면, “11분“이 가장 빈번하게 잠들어 있던 ‘분’. 그럼 답은 20 * 11 = 220.
(defn solve-part-1 [input]
  (let [{:keys [guard-id most-sleep-min]} (->> input
                                               generate-sleep-mins
                                               (sort-by #(count (val %)))
                                               last
                                               get-most-sleep-min)]
    (* guard-id (key most-sleep-min))))

(comment
  (solve-part-1 input))

;;[part 2]
;;주어진 분(minute)에 가장 많이 잠들어 있던 가드의 ID과 그 분(minute)을 곱한 값을 구하라.
(defn solve-part-2 [input]
  (let [{:keys [guard-id most-sleep-min]} (->> input
                                               generate-sleep-mins
                                               (map #(get-most-sleep-min %))
                                               (filter #(not-empty (:most-sleep-min %)))
                                               (sort-by #(val (:most-sleep-min %)))
                                               last)]
    (* guard-id (key most-sleep-min))))

(comment
  (solve-part-2 input))