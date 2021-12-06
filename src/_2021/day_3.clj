(ns _2021.day_3
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn parse [path]
  (->> path
       io/resource
       slurp
       s/split-lines))

(defn binary-string->integer
  [binary-string]
  (Integer/parseInt binary-string 2))

(defn most-frequent-value
  [v]
  (->> v
       frequencies
       (sort-by (juxt val key))
       last
       key))

(defn least-frequent-value
  [v]
  (->> v
       frequencies
       (sort-by (juxt val key))
       first
       key))

(defn calculate
  [{:keys [report most found filtering] :or {found ""} :as input}]
  (let [v (->> report
               (map #(s/split % #""))
               (map #(nth % (count found))))
        newly-found (->> (if most
                           (most-frequent-value v)
                           (least-frequent-value v))
                         (str found))]
    (-> input
        (update :report (fn [r]
                          (if filtering
                            (filter #(s/starts-with? % newly-found) r)
                            r)))
        (assoc :found newly-found))))

(defn gamma-rate
  [report]
  (-> (iterate calculate {:report report :most true :filtering false})
      (nth (count (first report)))
      :found))

(defn epsilon-rate
  [report]
  (-> (iterate calculate {:report report :most false :filtering false})
      (nth (count (first report)))
      :found))

(defn oxygen-generator-rating
  [report]
  (-> (iterate calculate {:report report :most true :filtering true})
      (nth (count (first report)))
      :found))

(defn co2-scrubber-rating
  [report]
  (-> (iterate calculate {:report report :most false :filtering true})
      (nth (count (first report)))
      :found))

(comment
  (def diagnostic-report (parse "2021/day_3_input"))

  ;;[part 1] What is the power consumption of the submarine?
  (->> ((juxt gamma-rate epsilon-rate) diagnostic-report)
       (map binary-string->integer)
       (apply *))

  ;;[part 2] What is the life support rating of the submarine?
  (->> ((juxt oxygen-generator-rating co2-scrubber-rating) diagnostic-report)
     (map binary-string->integer)
     (apply *))

  ,)
