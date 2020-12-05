(ns me.noonian.aoc-2020.day4
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.spec.alpha :as s]
            [clojure.walk :as walk]))

(def example-input
  "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in")

(def input (slurp (io/resource "day4.txt")))

(defn parse-passport [s]
  (->> (str/split s #"\s")
       (map #(str/split % #":"))
       (into {})
       walk/keywordize-keys))

(defn parse-batch [s]
  (->> (str/split s #"\n\n")
       (map parse-passport)))

(def expected-fields #{:byr :iyr :eyr :hgt :hcl :ecl :pid :cid})

(defn has-correct-fields? [passport]
  (every? #(contains? passport %) (disj expected-fields :cid)))

(deftest part-1
  (is (= 2 (count (filter has-correct-fields? (parse-batch example-input)))))
  (is (= 237 (count (filter has-correct-fields? (parse-batch input))))))

(defn digits-between [length lower upper]
  (s/and string?
         #(= length (count %))
         #(<= lower (Long/parseLong %) upper)))

(s/def ::byr (digits-between 4 1920 2002))
(s/def ::iyr (digits-between 4 2010 2020))
(s/def ::eyr (digits-between 4 2020 2030))
(s/def ::hgt (s/and string?
                    (fn [s]
                      (when-let [[_ magnitude unit] (re-matches #"(\d+)(cm|in)" s)]
                        (if (= unit "cm")
                          (<= 150 (Long/parseLong magnitude) 193)
                          (<= 59 (Long/parseLong magnitude) 76))))))
(s/def ::hcl #(re-matches #"#([0-9a-f]{6,6})" %))
(s/def ::ecl #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"})
(s/def ::pid #(re-matches #"[0-9]{9,9}" %))

(s/def ::passport
  (s/keys :req-un [::byr ::iyr ::eyr ::hgt ::hcl ::ecl ::pid]))

(deftest part-2
  (is (= 172 (count (filter #(s/valid? ::passport %) (parse-batch input))))))
