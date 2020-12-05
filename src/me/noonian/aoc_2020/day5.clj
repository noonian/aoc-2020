(ns me.noonian.aoc-2020.day5
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(def input (slurp (io/resource "day5.txt")))

(defn shrink-region [[lower upper] c]
  (let [diff (long (Math/ceil (/ (- upper lower) 2)))]
    (if (or (= c \F) (= c \L))
      [lower (- upper diff)]
      [(+ lower diff) upper])))

(defn parse-assignment [s]
  (let [row (first (reduce shrink-region [0 127] (take 7 s)))
        column (first (reduce shrink-region [0 7] (drop 7 s)))]
    {:row row
     :column column
     :id (+ column (* row 8))}))

(defn find-seat [assignments]
  (loop [ids (sort (map :id assignments))]
    (let [[a b] ids
          diff (- b a)]
      (if (= 2 diff)
        (inc a)
        (recur (rest ids))))))

(deftest parsing
  (is (= {:row 70 :column 7 :id 567} (parse-assignment "BFFFBBFRRR")))
  (is (= {:row 14 :column 7 :id 119} (parse-assignment "FFFBBBFRRR")))
  (is (= {:row 102 :column 4 :id 820} (parse-assignment "BBFFBBFRLL"))))

(deftest part-1
  (is (= 888 (apply max (map (comp :id parse-assignment) (str/split-lines input))))))

(deftest part-2
  (is (= 522 (find-seat (map parse-assignment (str/split-lines input))))))
