(ns day13.claw_contraption
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn parse-numbers [line]
  (let [matches (re-seq #"\d+" line)]
    (map #(Integer/parseInt %) matches)))

(defn parse-section [section]
  (let [lines (string/split-lines section)
        [button-a button-b prize] (map parse-numbers lines)]
    {:a1 (first button-a) :b1 (first button-b) :c1 (first prize)
     :a2 (second button-a) :b2 (second button-b) :c2 (second prize)}))

(defn parse-input []
  (let [input (slurp (io/resource "day13.txt"))
        sections (string/split input #"\n\n")]
    (mapv parse-section sections)))

(defn calculate-tokens [{:keys [a1 b1 a2 b2 c1 c2]} constraint-fn]
  (let [divisor (- (* a1 b2) (* a2 b1))
        x (/ (- (* b2 c1) (* b1 c2)) divisor)
        y (/ (- (* a1 c2) (* a2 c1)) divisor)]
    (when (and (constraint-fn x) (constraint-fn y) (not (ratio? x)) (not (ratio? y)))
      (+ (* 3 x) y))))

(defn update-prize [claw-machine]
  (let [prize-increase 10000000000000]
    (-> claw-machine
        (update :c1 + prize-increase)
        (update :c2 + prize-increase))))

(defn part1 [claw-machines]
  (->> (map (fn [m] (calculate-tokens m #(<= 0 % 100))) claw-machines)
       (filter identity)
       (apply +)))

(defn part2 [claw-machines]
  (->> (map update-prize claw-machines)
       (map (fn [m] (calculate-tokens m #(>= % 0))))
       (filter identity)
       (apply +)))

(defn -main []
  (let [claw-machines (parse-input)]
    (println "Part 1:" (part1 claw-machines))
    (println "Part 2:" (part2 claw-machines))))
