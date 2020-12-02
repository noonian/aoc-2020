(ns me.noonian.aoc-2020.day2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(def example-password-list
  "1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc")

(defn parse-policy [s]
  (let [[_ lower upper char-str] (re-find #"([0-9]+)-([0-9]+) ([a-zA-Z])" s)]
    {:lower (Long/parseLong lower)
     :upper (Long/parseLong upper)
     :char (.charAt char-str 0)}))

(defn parse-line [s]
  (let [[policy-str password] (str/split s #": ")]
    {:policy (parse-policy policy-str)
     :password password}))

(defn parse-passwords [s]
  (map parse-line (str/split-lines s)))

(defn valid-occurs? [{:keys [policy password]}]
  (let [{:keys [upper lower char]} policy
        occurs (get (frequencies password) char)]
    (and occurs
         (<= lower occurs upper))))

(defmacro xor [a b]
  `(let [a# ~a
         b# ~b]
     (or (and a# (not b#))
         (and (not a#) b#))))

(defn valid-positions? [{:keys [policy password]}]
  (let [{:keys [upper lower char]} policy]
    (xor (= char (get password (dec lower)))
         (= char (get password (dec upper))))))

(def input (slurp (io/resource "day2.txt")))

;;; Tests

(deftest parsing
  (is (= {:policy {:lower 1 :upper 3 :char \a} :password "abcde"}
         (parse-line "1-3 a: abcde"))))

(deftest validity
  (testing "occurance based policies"
    (is (valid-occurs? (parse-line "1-3 a: abcde")))
    (is (valid-occurs? (parse-line "2-9 c: ccccccccc")))
    (is (not (valid-occurs? "1-3 b: cdefg"))))
  (testing "position based policies"
    (is (valid-positions? (parse-line "1-3 a: abcde")))
    (is (not (valid-positions? (parse-line "1-3 b: cdefg"))))
    (is (not (valid-positions? (parse-line "2-9 c: ccccccccc"))))))

(deftest part-1
  (is (= 666 (count (filter valid-occurs? (parse-passwords input))))))

(deftest part-2
  (is (= 670 (count (filter valid-positions? (parse-passwords input))))))
