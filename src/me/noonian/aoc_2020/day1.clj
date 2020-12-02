(ns me.noonian.aoc-2020.day1
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]))

(def input (slurp (io/resource "day1.txt")))

(defn parse-report [s]
  (map #(Long/parseLong %) (str/split s #"\n")))

(defn solve [expense-report]
  (let [nums (parse-report expense-report)]
    (->> (for [a nums, b nums
               :when (= 2020 (+ a b))]
           #{a b})
         first
         (apply *))))

(defn solve3 [expense-report]
  (let [nums (parse-report expense-report)]
    (->> (for [a nums, b nums, c nums
               :when (= 2020 (+ a b c))]
           #{a b c})
         first
         (apply *))))

(def example-report
  "1721
979
366
299
675
1456")

(deftest part-1
  (is (= 514579 (solve example-report)))
  (is (= 996075 (solve input))))

(deftest part-2
  (is (= 241861950 (solve3 example-report)))
  (is (= 51810360 (solve3 input))))

;;; TODO
;;
;; - make generic solve function that takes number of factors as
;;   argument
