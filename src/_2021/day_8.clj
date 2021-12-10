(ns _2021.day_8
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.set :as set]))

(defn ->entry
  [line]
  (let [[a b] (s/split line #"\|")]
    {:note   (->> (re-seq #"\w+" a)
                  (group-by count)
                  (reduce (fn [acc [k v]]
                            (assoc acc k (->> (s/join "" v)
                                              frequencies
                                              (group-by val)
                                              (reduce (fn [acc [k v]]
                                                        (assoc acc k (set (keys v))))
                                                      {}))))
                          {}))
     :output (re-seq #"\w+" b)}))

(defn parse [path]
  (map ->entry (-> path
                   io/resource
                   slurp
                   s/split-lines)))

(defn careful-analysis
  [note]
  (let [a (first (set/difference (get-in note [3 1]) (get-in note [2 1])))
        c (first (set/intersection (get-in note [2 1]) (get-in note [6 2])))
        f (first (disj (get-in note [2 1]) c))
        d (first (disj (set/intersection (get-in note [4 1]) (get-in note [6 2])) c))
        b (first (set/difference (get-in note [4 1]) (set #{c d f})))
        e (first (set/difference (get-in note [6 2]) (set #{c d})))
        g (first (set/difference (get-in note [7 1]) (set #{a b c d e f})))]
    {a \a b \b c \c d \d e \e f \f g \g}))

(defn string->number-char
  [string]
  (get {"abcefg"  \0
        "cf"      \1
        "acdeg"   \2
        "acdfg"   \3
        "bcdf"    \4
        "abdfg"   \5
        "abdefg"  \6
        "acf"     \7
        "abcdefg" \8
        "abcdfg"  \9} string))

(defn ->fixed-number
  [analysis string]
  (->> (seq string)
       (map #(get analysis %))
       (sort-by #(int %))
       (apply str)
       string->number-char))

(defn note->fixed-output-numbers
  [{:keys [note output]}]
  (let [analysis (careful-analysis note)]
    (->> output
         (map #(->fixed-number analysis %))
         (apply str)
         Integer/parseInt)))

(comment
  (def notes (parse "2021/day_8_input"))

  ;;[part 1] In the output values, how many times do digits 1, 4, 7, or 8 appear?
  (->> notes
       (map :output)
       flatten
       (filter (fn [out]
                 (let [length (count out)]
                   (or (= 2 length)
                       (= 4 length)
                       (= 3 length)
                       (= 7 length)))))
       count)

  ;;[part 2] What do you get if you add up all of the output values?
  (->> notes
       (map note->fixed-output-numbers)
       (apply +))

  ,)

