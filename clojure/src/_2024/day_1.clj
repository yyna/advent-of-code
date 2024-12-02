(ns _2024.day-1
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn parse
  [path]
  (-> path
      (io/resource)
      (slurp)
      (string/split-lines)))

(defn ->two-digit-number [s]
  (let [[[_ n1 n2]] (re-seq #"(\d+)   (\d+)" s)]
    [(parse-long n1) (parse-long n2)]))

(comment
  (def input (->> (parse "2024/day_1_input")
                  (map ->two-digit-number)))

  ;; [part 1] What is the total distance between your lists?
  (->> (map -
            (sort (map first input))
            (sort (map second input)))
       (map abs)
       (apply +))

  ;; [part 2] Once again consider your left and right lists. What is their similarity score?
  (->> (map (fn [number]
              (* number
                 (or (get (frequencies (map second input))
                          number)
                     0)))
            (map first input))
       (apply +))

  :rcf)
