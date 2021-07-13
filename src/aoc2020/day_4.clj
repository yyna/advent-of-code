(ns aoc2020.day_4
  (:require [clojure.java.io :as io])
  (:require [clojure.spec.alpha :as s]))

(defn parse [input]
  "multiline string input 을 정수 sequence 로 변경해 return 하는 function"
  (->> (clojure.string/split input #"\n\n")))

(def input (->> "aoc2020/day_4_input"
                io/resource
                slurp
                parse))

(defn to-passport-map [input]
  "여권 정보 string 을 map 으로 변경해 return 하는 function
  (eyr:2021 hgt:184cm pid:431054313 hcl:#ceb3a1 cid:109 byr:1977 ecl:blu) -> {:pid \"#2a21e0\", :ecl \"#1b9b27\", :hgt {:value 165, :unit \"in\"}, :byr 1998, :iyr 2014, :eyr 2032}"
  (->> input
       (re-seq #"(byr|iyr|eyr|hgt|hcl|ecl|pid|cid):([a-zA-Z0-9#]+)")
       (reduce (fn [m [_ k v]]
                 (assoc
                   m
                   (keyword k)
                   (cond
                     (or (= k "iyr") (= k "byr") (= k "eyr")) (Integer/parseInt v)
                     (= k "hgt") (let [[value unit] (rest (re-find (re-matcher #"([\d]+)?([a-z]+)?" v)))]
                                   {:value (Integer/parseInt value) :unit unit})
                     :else v)))
               {})))

(defn between [a b num]
  "num 이 a 와 b 사이의 값인지 판별하는 function"
  (and (>= num a) (<= num b)))

(s/def ::byr #(between 1920 2002 %))
(s/def ::iyr #(between 2010 2020 %))
(s/def ::eyr #(between 2020 2030 %))
(s/def ::hgt (fn [{v :value u :unit}]
               (or
                 (and (= u "cm") (between 150 193 v))
                 (and (= u "in") (between 59 76 v)))))
(s/def ::hcl #(re-matches #"#[0-9a-f]{6}" %))
(s/def ::ecl #(re-matches #"(amb|blu|brn|gry|grn|hzl|oth)" %))
(s/def ::pid #(re-matches #"0[\d]{8}" %))
(s/def ::cid int?)
(s/def ::passport (s/keys :req [::iyr ::eyr ::hgt ::hcl ::ecl ::pid]
                          :opt [::cid]))

(defn has-all-required-keys [passport]
  "passport 에 required keys 정보가 모두 있는지 확인해 boolean 을 return 하는 function"
  (->> (clojure.set/difference
         #{:ecl :byr :iyr :hgt :pid :hcl :eyr}
         (set (keys passport)))
       empty?))

(defn is-valid-passport [passport]
  (if (has-all-required-keys passport)
    (let [{iyr :iyr eyr :eyr hgt :hgt hcl :hcl ecl :ecl pid :pid cid :cid} passport]
      (if (s/valid? ::passport {::iyr iyr ::eyr eyr ::hgt hgt ::hcl hcl ::ecl ecl ::pid pid ::cid cid})
        passport))))

(comment
  (->> input
       (map to-passport-map)
       (map is-valid-passport)))

;;[part 1]
;; 주어진 입력에서 '유효한' 여권의 숫자를 반환하여라.
(defn solve-part-1 [input]
  (->> input
       (map to-passport-map)
       (map has-all-required-keys)
       (filter #(= true %))
       count))

(comment
  (solve-part-1 input))

;;[part 2]