(ns me.noonian.aoc-2020.day9
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(defn find-addends [n nums]
  (first
   (for [a nums b nums
         :when (and (= n (+ a b))
                    (not= a b))]
     [a b])))

(defn parse-nums [s]
  (map #(Long/parseLong %) (str/split-lines s)))

(defn first-anomaly [preamble-length nums]
  (let [[preamble others] (split-at preamble-length nums)]
    (loop [prev (into [] preamble)
           [n & others] others]
      (when n
        (if-not (find-addends n prev)
          n
          (recur (conj (subvec prev 1) n)
                 others))))))

(defn segment [n nums]
  (loop [res []
         others (seq nums)
         sum 0]
    (when others
      (if (= sum n)
        res
        (recur (conj res (first others))
               (next others)
               (+ sum (first others)))))))

(defn find-segment [n nums]
  (when (seq nums)
    (or (segment n nums)
        (recur n (next nums)))))

(defn encryption-weakness [preamble-length nums]
  (let [n (first-anomaly preamble-length nums)
        seg (find-segment n nums)]
    (+ (apply min seg)
       (apply max seg))))

(def example-input
  "35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576")

(def input (slurp (io/resource "day9.txt")))

(deftest part-1
  (is (= 127 (first-anomaly 5 (parse-nums example-input))))
  (is (= 15353384 (first-anomaly 25 (parse-nums input)))))

(deftest part-2
  (is (= 62 (encryption-weakness 5 (parse-nums example-input))))
  (is (= 2466556 (encryption-weakness 25 (parse-nums input)))))
