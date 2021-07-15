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

;(def input (->> "nop +0\nacc +1\njmp +4\nacc +3\njmp -3\nacc -99\nacc +1\njmp -4\nacc +6"
;                parse))

(defn execute [input]
  "input 된 list 를 한번 실행한 후 결과를 return 하는 function"
  (let [{:keys [list index]} input
        {:keys [operator operand]} (nth list index)]
    (-> input
        (update :index #(mod (+ % (if (= operator "jmp") operand 1)) (count list)))
        (update :value #(+ % (if (= operator "acc") operand 0)))
        (update :visit-set #(conj % index)))))

(defn infinite? [input]
  "현재 list 의 상태가 infinite 인지 확인 하는 function, 확인할 수 없으면 nil 이 return 됨"
  (let [{:keys [list index visit-set]} input
        operator (:operator (last list))]
    (cond
      (and (not= operator "jmp") (visit-set (- (count list) 1))) false
      (visit-set index) true)))

(defn generate-states [input]
  "infinite 여부를 확인하기 직전부터의 states 를 return 하는 function"
  (->> {:list input :index 0 :value 0 :visit-set #{}}
       (iterate execute)
       (drop-while #(nil? (infinite? %)))))

;;[part 1]
(defn solve-part-1 [input]
  (->> input
       generate-states
       first
       :value))

(comment
  (solve-part-1 input))

(defn generate-all-cases [input]
  (->> (for [i (range (count input))]
         (let [{:keys [operator operand]} (nth input i)]
           (case operator
             "nop" (assoc input i {:operator "jmp" :operand operand})
             "jmp" (assoc input i {:operator "nop" :operand operand})
             nil)))
       (filter #(seq %))))

;;[part 2]
(defn solve-part-2 [input]
  (->> input
       generate-all-cases
       (map #(->> %
                  generate-states
                  second))
       (filter #(->> %
                     infinite?
                     (= false)))
       first
       :value))

(comment
  (solve-part-2 input))
