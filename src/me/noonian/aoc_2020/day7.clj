(ns me.noonian.aoc-2020.day7
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(defn parse-rule [s]
  (if-let [[_ color] (re-find #"^(.+) bags contain no other bags." s)]
    {:color color :leaf? true}
    (let [[_ color contents-str :as t] (re-find #"(.+) bags contain (.+)." s)
          contents (into {}
                     (for [s (str/split contents-str #", ")
                           :let [[_ n color] (re-find #"([0-9]+) (.+) bag" s)]]
                       [color (Long/parseLong n)]))]
      {:color color :contents contents})))

(defn parse-rules [s]
  (into {}
    (for [{:keys [color] :as rule} (mapv parse-rule (str/split-lines s))]
      [color rule])))

(defn select-bags [rules colors] (into [] (vals (select-keys rules colors))))

(defn select-bag [rules color] (get rules color))

(defn can-contain? [rules target-color {:keys [color contents] :as bag}]
  (if (contains? contents target-color)
    true
    (some #(can-contain? rules target-color %)
          (select-bags rules (keys contents)))))

(defn parents [rules color]
  (filter #(can-contain? rules color %) (vals rules)))

(defn count-bags [rules color]
  (let [{:keys [color contents leaf?] :as bag} (select-bag rules color)]
    (if leaf?
      0
      (->> (for [[color n] contents]
             (+ n (* n (count-bags rules color))))
           (apply + 0)))))

(def example-input
  "light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.")

(def part2-example
  "shiny gold bags contain 2 dark red bags.
dark red bags contain 2 dark orange bags.
dark orange bags contain 2 dark yellow bags.
dark yellow bags contain 2 dark green bags.
dark green bags contain 2 dark blue bags.
dark blue bags contain 2 dark violet bags.
dark violet bags contain no other bags.")

(def input (slurp (io/resource "day7.txt")))

(deftest parsing
  (is (= {:color "faded blue" :leaf? true}
         (parse-rule "faded blue bags contain no other bags.")))
  (is (= {:color "vibrant plum" :contents {"faded blue" 5 "dotted black" 6}}
         (parse-rule "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags."))))

(deftest part-1
  (is (= 4 (count (parents (parse-rules example-input) "shiny gold"))))
  (is (= 208 (count (parents (parse-rules input) "shiny gold")))))

(deftest part-2
  (is (= 32 (count-bags (parse-rules example-input) "shiny gold")))
  (is (= 126 (count-bags (parse-rules part2-example) "shiny gold")))
  (is (= 1664 (count-bags (parse-rules input) "shiny gold"))))
