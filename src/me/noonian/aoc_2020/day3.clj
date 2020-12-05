(ns me.noonian.aoc-2020.day3
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(def example-input
  "..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#")

(def input (slurp (io/resource "day3.txt")))

(defn parse-map [s]
  (->> s
       str/split-lines
       (mapv #(into [] %))))

(defn add-vector [[x1 y1] [x2 y2]]
  [(+ x1 x2) (+ y1 y2)])

(defn lookup [map [x y :as pos]]
  (let [x-max (count (first map))
        x (mod x x-max)]
    (get-in map [y x])))

(defn tree? [val] (= val \#))

(defn count-trees [map slope]
  (loop [trees 0
         pos slope]
    (if-let [val (lookup map pos)]
      (let [new-pos (add-vector pos slope)]
        (recur (cond-> trees
                 (tree? val) inc)
               new-pos))
      trees)))

(def part2-slopes
  [[1 1]
   [3 1]
   [5 1]
   [7 1]
   [1 2]])

(deftest part-1
  (is (= 7 (count-trees (parse-map example-input) [3 1])))
  (is (= 259 (count-trees (parse-map input) [3 1]))))

(deftest part-2
  (is (= 336 (apply * (map (partial count-trees (parse-map example-input)) part2-slopes))))
  (is (= 2224913600 (apply * (map (partial count-trees (parse-map input)) part2-slopes)))))
