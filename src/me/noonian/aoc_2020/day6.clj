(ns me.noonian.aoc-2020.day6
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.test :refer [deftest is testing]]))

(def example-input
  "abc

a
b
c

ab
ac

a
a
a
a

b")

(def input (slurp (io/resource "day6.txt")))

(defn count-answers [group]
  (->> (str/split-lines group)
       (apply str)
       (distinct)
       count))

(defn count-intersecting [group]
  (->> (str/split-lines group)
       (map set)
       (apply set/intersection)
       count))

(deftest part-1
  (is (= 11 (reduce + 0 (map count-answers (str/split example-input #"\n\n")))))
  (is (= 6630 (reduce + 0 (map count-answers (str/split input #"\n\n"))))))

(deftest part-2
  (is (= 6 (reduce + 0 (map count-intersecting (str/split example-input #"\n\n")))))
  (is (= 3437 (reduce + 0 (map count-intersecting (str/split input #"\n\n"))))))
