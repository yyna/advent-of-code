(ns _2021.day_10
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn parse [path]
  (-> path
      io/resource
      slurp
      s/split-lines))

(defn closing
  [o]
  (case o
    \( \)
    \[ \]
    \{ \}
    \< \>))

(defn match?
  [a b]
  (= (closing a) b))

(defn syntax-error-score
  [c]
  (case c
    \) 3
    \] 57
    \} 1197
    \> 25137))

(defn check-line
  [line]
  (->> line
       seq
       (reduce (fn [stack c]
                 (if (contains? #{\( \[ \{ \<} c)
                   (conj stack c)
                   (if (match? (peek stack) c)
                     (pop stack)
                     (reduced c))))
               [])))

(defn complete-by-adding
  [stack]
  (->> stack
       (map closing)
       reverse
       (apply str)))

(defn autocomplete-score
  [c]
  (case c
    \) 1
    \] 2
    \} 3
    \> 4))

(defn total-score
  [str]
  (->> str
       seq
       (map autocomplete-score)
       (reduce #(+ (* %1 5) %2))))

(defn middle-score
  [scores]
  (nth (into [] (sort scores)) (/ (dec (count scores)) 2)))

(comment
  (def lines (parse "2021/day_10_input"))

  ;;[part 1] What is the total syntax error score for those errors?
  (->> lines
       (map check-line)
       (filter char?)
       (map syntax-error-score)
       (apply +))

  ;;[part 2] What is the middle score?
  (->> lines
       (map check-line)
       (filter vector?)
       (map complete-by-adding)
       (map total-score)
       middle-score)

  ,)