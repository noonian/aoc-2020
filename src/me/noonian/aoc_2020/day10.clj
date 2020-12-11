(ns me.noonian.aoc-2020.day10
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(defn parse-nums [s] (mapv #(Long/parseLong %) (str/split-lines (str/trim s))))

(defn diffs [nums]
  (loop [out 0
         diffs []
         nums (seq (sort nums))]
    (if-not nums
      (conj diffs 3)
      (recur (first nums)
             (conj diffs (- (first nums) out))
             (next nums)))))

(defn contribution-factor [n]
  (condp = n
    1 1
    2 2
    3 4
    (+ (contribution-factor (dec n))
       (contribution-factor (- n 2))
       (contribution-factor (- n 3)))))

(defn count-arrangements [gaps]
  (if-not (seq gaps)
    1
    (let [[ones others] (split-with #(= 1 %) gaps)]
      (if-not (seq ones)
        (recur (next others))
        (* (contribution-factor (count ones))
           (count-arrangements others))))))

(def example-input
  "28
33
18
42
31
14
46
20
48
47
24
23
49
45
19
38
39
11
1
32
25
35
8
17
7
9
4
2
34
10
3")

(def input (slurp (io/resource "day10.txt")))

(deftest part-1
  (let [freqs (frequencies (diffs (parse-nums example-input)))]
    (is (= 220 (* (get freqs 3) (get freqs 1)))))
  (let [freqs (frequencies (diffs (parse-nums input)))]
    (is (= 2432 (* (get freqs 3) (get freqs 1))))))

(deftest part-2
  (is (= 19208 (count-arrangements (diffs (parse-nums example-input)))))
  (is (= 453551299002368 (count-arrangements (diffs (parse-nums input))))))
