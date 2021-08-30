(ns aoc2020.day_8
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse [input]
  "multiline string input 을 정수 sequence 로 변경해 return 하는 function"
  (->> (str/split-lines input)
       (mapv (fn [line]
               (let [[operator operand] (str/split line #" ")]
                 {:operator operator
                  :operand (Integer/parseInt operand)})))))

(def input (->> "aoc2020/day_8_input"
                io/resource
                slurp
                parse))
;({:operator acc :operand 7}
; {:operator acc :operand 23}
; {:operator acc :operand 41})
; ...)

;(def input (->> "nop +0\nacc +1\njmp +4\nacc +3\njmp -3\nacc -99\nacc +1\nnop -4\nacc +6"
;                parse))

(defn execute [input]
  "input 된 list 를 한번 실행한 후 결과를 return 하는 function"
  (let [{:keys [list index]} input
        {:keys [operator operand]} (nth list index)]
    (-> input
        (update :index #(+ % (if (= operator "jmp") operand 1)))
        (update :value #(+ % (if (= operator "acc") operand 0)))
        (update :visit-set #(conj % index)))))

(defn infinite? [input]
  "현재 list 의 상태가 infinite 인지 확인 하는 function, 확인할 수 없으면 nil 이 return 됨"
  (let [{:keys [list index visit-set]} input]
    (cond
      (>= index (count list)) false
      (visit-set index) true)))

;;[part 1]
(defn solve-part-1 [input]
  (->> {:list input :index 0 :value 0 :visit-set #{}}
       (iterate execute)
       (take-while #(nil? (infinite? %)))
       last
       :value))

(comment
  (solve-part-1 input))

;;[part 2]
(defn generate-all-cases [input]
  (->> (for [i (range (count input))]
         (let [{:keys [operator operand]} (nth input i)]
           (case operator
             "nop" (assoc input i {:operator "jmp" :operand operand})
             "jmp" (assoc input i {:operator "nop" :operand operand})
             nil)))
       (filter #(seq %))))

(defn execute-to-infinity-or-to-the-end [input]
  (->> {:list input :index 0 :value 0 :visit-set #{}}
       (iterate execute)
       (drop-while #(nil? (infinite? %)))
       first))

(defn solve-part-2 [input]
  (->> input
       generate-all-cases
       (map #(execute-to-infinity-or-to-the-end %))
       (filter #(= false (infinite? %)))
       first
       :value))

(comment
  (solve-part-2 input))