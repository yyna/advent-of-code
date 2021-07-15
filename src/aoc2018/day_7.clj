(ns aoc2018.day-7 (:require [clojure.java.io :as io]))

(defn parse [input]
  "multiline string input 을 string sequence 로 변경해 return 하는 function"
  (->> input
       clojure.string/split-lines
       (map
         (fn [string]
           (let [[a b] (rest (re-find (re-matcher #"Step (\w) must be finished before step (\w) can begin." string)))]
             [a b])))))
;;([G W] [X S] [F V] [C Y] [M J] [K Z] [U W] [I H] [W B] [A Y] [Y D] [S Q] [N V] [H D] [D Q] [L E] [Q E] [T R] [J P] [R E] [E V] [O P] [P B] [Z V] [B V] [Y B] [C B] [Q T] [W P] [X Z] [L T] [G Y] [Y R] [E B] [X E] [Y V] [H L] [L J] [S T] [F T] [Y J] [A H] [P Z] [R O] [X F] [I O] [Y Q] [S D] [Q B] [C D] [Y N] [O Z] [G D] [A O] [U N] [Y P] [E O] [I Q] [W O] [D B] [Z B] [L B] [P V] [C E] [S O] [U T] [U O] [Y L] [N L] [Q Z] [U L] [U D] [J O] [L R] [S P] [H R] [F I] [D T] [C M] [W D] [R V] [U S] [K R] [D V] [D R] [I E] [L O] [T Z] [A E] [D Z] [H V] [A L] [W R] [F A] [Y Z] [I P] [F J] [H B] [G Z] [C K] [D E])

(def input (->> "aoc2018/day_7_input"
                io/resource
                slurp
                parse))

(defn build-graph [input]
  "주어진 input 으로부터 graph 를 생성해서 return 하는 function"
  (->> input
       (reduce
         (fn [m [a b]]
           (let [{to :to :or {to #{}}} (m a)
                 {from :from :or {from #{}}} (m b)]
             (assoc-in (assoc-in m [b :from] (conj from a)) [a :to] (conj to b))))
         {})))
;;{"T" {:to ["R" "Z"], :from ["Q" "L" "S" "F" "U" "D"]},
;; "K" {:to ["Z" "R"], :from ["C"]},
;; "Q" {:from ["S" "D" "Y" "I"], :to ["E" "T" "B" "Z"]},
;; ...}

(defn remove-vertex [graph vertex]
  "input graph 에서 input vertex 가 제거된 graph 를 return 하는 function"
  (def new-graph (->> (vec ((graph vertex) :to)) ; let 이름 이상함
                      (reduce (fn [g x]
                                (assoc-in g [x :from] (disj ((get g x) :from) vertex)))
                              graph)))
  (dissoc new-graph vertex))

(defn remove-vertices [graph vertices]
  (reduce (fn [g x] (remove-vertex g x)) graph vertices))

(defn find-available-vertex [graph maximum]
  "input graph 의 수행 가능한 n 개의 vertex 를 찾아 return 하는 function"
  (->> graph
       (filter (fn [[k v]] (= 0 (count (v :from)))))
       keys
       sort
       (take maximum)))

(defn remove-available-vertices [graph maximum]
  (let [vertices (find-available-vertex graph maximum)]
    {:vertices vertices :graph (remove-vertices graph vertices)}))

(defn solve-part-1 [graph]
  (loop [g graph order []]
    (let [{[vertex] :vertices graph :graph} (remove-available-vertices g 1)]
      (if (> (count graph) 0)
        (recur graph (conj order vertex))
        (apply str (conj order vertex))))))

(comment
  (->> input
       build-graph
       solve-part-1))

;;[part 2]