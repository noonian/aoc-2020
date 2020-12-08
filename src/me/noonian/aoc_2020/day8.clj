(ns me.noonian.aoc-2020.day8
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]])
  (:refer-clojure :exclude [eval]))

(defn parse-instruction [s]
  (let [[_ op arg] (re-find #"^([a-z]+) (.+)$" s)]
    {:op op :arg (Long/parseLong arg)}))

(defn parse-program [s]
  (mapv parse-instruction (str/split-lines s)))

(defmulti eval-op (fn [env {:keys [op]}] op))

(defmethod eval-op "nop" [env {:keys [arg]}]
  (update env :ip inc))

(defmethod eval-op "acc" [env {:keys [arg]}]
  (-> env
      (update :acc + arg)
      (update :ip inc)))

(defmethod eval-op "jmp" [env {:keys [arg]}]
  (update env :ip + arg))

(defn eval [program]
  (loop [{:keys [ip] :as env} {:ip 0 :acc 0 :program program}
         seen #{}]
    (cond
      (seen ip) (assoc env :repeat-ip ip)
      :else
      (if-let [op (get program ip)]
        (recur (eval-op env op) (conj seen ip))
        env))))

(defn swap-op [{:keys [op] :as instruction}]
  (if (= op "nop")
    (assoc instruction :op "jmp")
    (assoc instruction :op "nop")))

(defn all-versions [program]
  (let [ips (filter #(or (= "nop" (:op (get program %)))
                         (= "jmp" (:op (get program %))))
                    (range (count program)))]
    (cons program
          (for [ip ips]
            (update program ip swap-op)))))

(defn fix [program]
  (->> (all-versions program)
       (map eval)
       (filter (comp not :repeat-ip))
       first))

(def example-program
  "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6")

(def input (slurp (io/resource "day8.txt")))

(deftest part-1
  (is (= 5 (:acc (eval (parse-program example-program)))))
  (is (= 1446 (:acc (eval (parse-program input))))))

(deftest part-2
  (is (= 8 (:acc (fix (parse-program example-program)))))
  (is (= 1403 (:acc (fix (parse-program input))))))
