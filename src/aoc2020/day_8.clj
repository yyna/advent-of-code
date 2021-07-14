(ns aoc2020.day_8
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse [input]
  "multiline string input 을 정수 sequence 로 변경해 return 하는 function"
  (->> (str/split-lines input)
       (mapv (fn [line]
               (let [[operator operand] (str/split line #" ")]
                 {:operator operator
                  :operand (Integer/parseInt operand)
                  :visit 0})))))


(def input (->> "aoc2020/day_8_input"
                io/resource
                slurp
                parse))
;({:operator acc, :operand 7, :visit 0}
; {:operator acc, :operand 23, :visit 0}
; {:operator acc, :operand 41, :visit 0})
; ...)

;(def input (->> "nop +0\nacc +1\njmp +4\nacc +3\njmp -3\nacc -99\nacc +1\nnop -4\nacc +6"
;                parse))

(defn accumulate [input]
  "input 된 list 를 한번 accumulate 한 후 결과를 return 하는 function"
  (let [[list index value] ((juxt :list :index :value) input)
        [operator operand visit] ((juxt :operator :operand :visit) (nth list index))]
    {:list (assoc-in list [index :visit] (inc visit))
     :index (mod (+ (if (= operator "jmp") (+ index operand) (inc index))) (count list))
     :value (+ value (if (= operator "acc") operand 0))}))

(defn infinite? [input]
  "현재 list 의 상태가 infinite 인지 확인 하는 function, 확인할 수 없으면 nil 이 return 됨"
  (let [[list index] ((juxt :list :index) input)
        [operator visit] ((juxt :operator :visit) (last list))
        duplicate-visit (count (filter #(> (% :visit) 1) list))]
    (cond
      (and (= index 0) (not= operator "jmp") (= visit 1)) false
      (> duplicate-visit 0) true)))

(defn get-last-value [input]
  "infinite 인지 확인된 상태에서의 accumulator value 및 infinite 여부를 return 하는 function"
  (let [states (take-while #(nil? (infinite? %)) (iterate accumulate {:list input :index 0 :value 0}))
        last-state (last states)
        next-state (accumulate last-state)]
    {:last-value (:value last-state)
     :next-value (:value next-state)
     :infinite? (infinite? next-state)}))

;;[part 1]
(defn solve-part-1 [input]
  (->> input
       get-last-value
       :last-value))

(comment
  (solve-part-1 input))

(defn generate-all-cases [input]
  (->> (for [i (range (count input))]
         (let [[operator operand] ((juxt :operator :operand) (nth input i))]
           (cond
             (= operator "nop") (assoc input i {:operator "jmp" :operand operand :visit 0})
             (= operator "jmp") (assoc input i {:operator "nop" :operand operand :visit 0}))))
       (filter #(seq %))))

;;[part 2]
(defn solve-part-2 [input]
  (->> input
       generate-all-cases
       (map get-last-value)
       (filter #(= false (% :infinite?)))
       last
       :next-value))

(comment
  (solve-part-2 input))
