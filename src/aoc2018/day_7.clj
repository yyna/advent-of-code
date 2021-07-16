(ns aoc2018.day-7 (:require [clojure.java.io :as io]))

(defn parse [input]
  "multiline string input 을 string sequence 로 변경해 return 하는 function"
  (->> input
       clojure.string/split-lines
       (mapv
         (fn [string]
           (let [[a b] (rest (re-find (re-matcher #"Step (\w) must be finished before step (\w) can begin." string)))]
             [(first a) (first b)])))))
;;[[G W] [X S] [F V] [C Y] [M J] [K Z] [U W] [I H] [W B] [A Y] [Y D] [S Q] [N V] [H D] [D Q] [L E] [Q E] [T R] [J P] [R E] [E V] [O P] [P B] [Z V] [B V] [Y B] [C B] [Q T] [W P] [X Z] [L T] [G Y] [Y R] [E B] [X E] [Y V] [H L] [L J] [S T] [F T] [Y J] [A H] [P Z] [R O] [X F] [I O] [Y Q] [S D] [Q B] [C D] [Y N] [O Z] [G D] [A O] [U N] [Y P] [E O] [I Q] [W O] [D B] [Z B] [L B] [P V] [C E] [S O] [U T] [U O] [Y L] [N L] [Q Z] [U L] [U D] [J O] [L R] [S P] [H R] [F I] [D T] [C M] [W D] [R V] [U S] [K R] [D V] [D R] [I E] [L O] [T Z] [A E] [D Z] [H V] [A L] [W R] [F A] [Y Z] [I P] [F J] [H B] [G Z] [C K] [D E]]

(def input (->> "aoc2018/day_7_input"
                io/resource
                slurp
                parse))

;(def input (->> "Step C must be finished before step A can begin.\nStep C must be finished before step F can begin.\nStep A must be finished before step B can begin.\nStep A must be finished before step D can begin.\nStep B must be finished before step E can begin.\nStep D must be finished before step E can begin.\nStep F must be finished before step E can begin."
;                parse))

(defn char->second [char]
  (- (int char) 4))

(defn build-graph [input]
  "주어진 input 으로부터 graph 를 생성해서 return 하는 function"
  (->> input
       (reduce
         (fn [m [a b]]
           (let [{to :to :or {to #{}}} (m a)
                 {from :from :or {from #{}}} (m b)]
             (-> m
                 (assoc-in [b :from] (conj from a))
                 (assoc-in [a :to] (conj to b)))))
         {})
       (reduce
         (fn [m [k {:keys [to from]}]]
           (assoc m k {:id k :from from :to to :time (char->second k)}))
         {})))
;{\A {:id \A, :from #{\F}, :to #{\E \H \L \O \Y}, :time 61},
; \B {:id \B, :from #{\C \D \E \H \L \P \Q \W \Y \Z}, :to #{\V}, :time 62},
; \C {:id \C, :from nil, :to #{\B \D \E \K \M \Y}, :time 63},}

(defn remove-vertex-by-id [graph id]
  "input graph 에서 input vertex 가 제거된 graph 를 return 하는 function"
  (->> ((graph id) :to)
       (reduce (fn [g x]
                 (assoc-in g [x :from] (disj ((get g x) :from) id)))
               (dissoc graph id))))

(defn remove-vertices [graph vertices]
  (->> vertices
       (reduce (fn [g x]
                 (remove-vertex-by-id g (first x)))
               graph)))

(defn find-available-vertex [graph max]
  "input graph 의 수행 가능한 n 개의 vertex 를 찾아 return 하는 function"
  (->> graph
       (filter #(empty? ((val %) :from)))
       (sort-by key)
       (take max)
       (mapv #((juxt :id :time) (val %)))))

(defn remove-available-vertices [graph n]
  (let [vertices (find-available-vertex graph n)]
    {:vertices vertices :graph (remove-vertices graph vertices)}))

(defn solve-part-1 [graph]
  (loop [g graph order []]
    (let [{[vertex] :vertices graph :graph} (remove-available-vertices g 1)]
      (if (> (count graph) 0)
        (recur graph (conj order (first vertex)))
        (apply str (conj order (first vertex)))))))

(comment
  (->> input
       build-graph
       solve-part-1))

;;[part 2]
(defn assign [status works]
  "현재 status 를 확인하여 works 를 assign 하는 함수
  status = [[C 3] [] []] works = [[C 5] [B 5] []] -> [[C 3] [B 5] []]"
  (let [wip (->> status
                 (filterv #(seq %)))]
    (reduce (fn [v x]
              (let [wip-set (->> v
                                 (filterv #(seq %))
                                 (map #(first %))
                                 set)
                    available-works (->> works
                                         (filterv #(nil? (wip-set (first %)))))]
                (cond
                  (seq x) v
                  (empty? available-works) (conj v [])
                  :else (conj v (first available-works)))))
            wip status)))

(defn in-1-second [graph status]
  (let [new-status-with-zero (->> status
                                  (mapv (fn [x]
                                          (if-let [[id left] (seq x)]
                                            [id (dec left)]
                                            []))))
        new-graph (->> new-status-with-zero
                       (filterv #(= 0 (second %)))
                       (reduce (fn [g [id]]
                                 (remove-vertex-by-id g id))
                               graph))
        new-status (->> new-status-with-zero
                        (reduce (fn [s x]
                                  (conj s
                                    (if (or (empty? x) (= (second x) 0))
                                      []
                                      x)))
                                []))]
      {:graph new-graph
       :status new-status}))

(defn work [{:keys [time-elapsed] :as all}]
  (let [{:keys [graph status]} (in-1-second (:graph all) (:status all))]
    {:graph graph
     :status (assign status (find-available-vertex graph (count status)))
     :time-elapsed (inc time-elapsed)}))

(defn solve-part-2 [input]
  (->> (take-while
         #(seq (% :graph))
         (iterate work {:graph (build-graph input)
                        :status [[] [] [] [] []]
                        :time-elapsed 0}))
       last
       :time-elapsed))

(comment
  (solve-part-2 input))