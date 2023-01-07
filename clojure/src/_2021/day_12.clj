(ns _2021.day_12
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.set :as set]))

(defn parse [path]
  (let [lines (-> path
                  io/resource
                  slurp
                  s/split-lines)]
    (->> lines
         (map #(s/split % #"-"))
         (reduce (fn [acc [_a _b]]
                   (let [a (keyword _a)
                         b (keyword _b)]
                     (cond-> acc
                       (a acc) (update a #(conj % b))
                       (nil? (a acc)) (assoc a #{b})
                       (b acc) (update b #(conj % a))
                       (nil? (b acc)) (assoc b #{a}))))
                 {}))))

(defn small-cave?
  [cave]
  (= (s/lower-case (name cave)) (name cave)))

(defn next-caves-to-visit
  [connections current-cave small-cave-twice? visited]
  (let [caves-visit-twice (->> (frequencies visited)
                               (filter (fn [[k v]]
                                         (and (small-cave? k)
                                              (> v 1)))))
        connected-caves (disj (current-cave connections) :start)
        visited-small-caves (filter small-cave? visited)]
    (if (or (not small-cave-twice?) (seq caves-visit-twice))
      (set/difference connected-caves (set visited-small-caves))
      connected-caves)))

(defn ->tree
  [connections cave-to-visit small-cave-twice? visited]
  (let [next-visited (conj visited cave-to-visit)
        next-caves (next-caves-to-visit connections cave-to-visit small-cave-twice? next-visited)]
    (if (= :end cave-to-visit)
      (list :end)
      (-> (keep #(->tree connections % small-cave-twice? next-visited) next-caves)
          (conj cave-to-visit)))))

(comment
  (def connections (parse "2021/day_12_input"))

  ;;[part 1] How many paths through this cave system are there that visit small caves at most once?
  (->> (->tree connections :start false [])
       (tree-seq seq? identity)
       (filter #(= % :end))
       count)

  ;;[part 2] how many paths through this cave system are there?
  (->> (->tree connections :start true [])
       (tree-seq seq? identity)
       (filter #(= % :end))
       count)

  ,)