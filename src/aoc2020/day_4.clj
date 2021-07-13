(ns aoc2020.day_4
  (:require [clojure.java.io :as io]
            [clojure.spec.alpha :as s]))

(defn parse [input]
  "multiline string input 을 정수 sequence 로 변경해 return 하는 function"
  (->> (clojure.string/split input #"\n\n")))

(def input (->> "aoc2020/day_4_input"
                io/resource
                slurp
                parse))

(defn string->passport-map [string]
  "여권 정보 string 을 map 으로 변경해 return 하는 function
  (eyr:2021 hgt:184cm pid:431054313 hcl:#ceb3a1 cid:109 byr:1977 ecl:blu) -> {:pid \"#2a21e0\", :ecl \"#1b9b27\", :hgt {:value 165, :unit \"in\"}, :byr 1998, :iyr 2014, :eyr 2032}"
  (->> string
       (re-seq #"(byr|iyr|eyr|hgt|hcl|ecl|pid|cid):([a-zA-Z0-9#]+)")
       (reduce (fn [m [_ k v]]
                 (assoc
                   m
                   (keyword k)
                   (cond
                     (or (= k "iyr") (= k "byr") (= k "eyr") (= k "cid")) (Integer/parseInt v)
                     (= k "hgt") (let [[value unit] (rest (re-find (re-matcher #"([\d]+)?([a-z]+)?" v)))]
                                   {:value (Integer/parseInt value) :unit unit})
                     :else v)))
               {})))

(s/def ::byr (s/int-in 1920 2003))
(s/def ::iyr (s/int-in 2010 2021))
(s/def ::eyr (s/int-in 2020 2031))
(s/def ::hgt (fn [{v :value u :unit}]
               (or
                 (and (= u "cm") (<= 150 v 193))
                 (and (= u "in") (<= 59 v 76)))))
(s/def ::hcl #(re-matches #"#[0-9a-f]{6}" %))
(s/def ::ecl #(re-matches #"(amb|blu|brn|gry|grn|hzl|oth)" %))
(s/def ::pid #(re-matches #"\d{9}" %))
(s/def ::cid number?)
(s/def ::passport (s/keys :req-un [::byr ::iyr ::eyr ::hgt ::hcl ::ecl ::pid]
                          :opt-un [::cid]))

(defn has-all-required-keys? [passport]
  "passport 에 required keys 정보가 모두 있는지 확인해 boolean 을 return 하는 function"
  (->> (clojure.set/difference
         #{:ecl :byr :iyr :hgt :pid :hcl :eyr}
         (set (keys passport)))
       empty?))

(defn valid-passport? [passport]
  "valid 한 passport 인지 확인해 boolean 을 return 하는 function"
  (s/valid? ::passport passport))

;;[part 1]
;; 주어진 입력에서 '유효한' 여권의 숫자를 반환하여라.
(defn solve-part-1 [input]
  (->> input
       (map string->passport-map)
       (filter has-all-required-keys?)
       count))

(comment
  (solve-part-1 input))

;;[part 2]
(defn solve-part-2 [input]
  (->> input
       (map string->passport-map)
       (filter valid-passport?)
       count))

(comment
  (solve-part-2 input))