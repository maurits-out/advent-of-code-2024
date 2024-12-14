(ns day07.core
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.math :as math]))

(defn parse-line [line]
  (let [parts (string/split line #":? ")
        numbers (map #(parse-long %) parts)]
    {:test-value (first numbers) :numbers (vec (rest numbers))}))

(defn parse-input []
  (->> (slurp (io/resource "day07.txt"))
       (string/split-lines)
       (mapv parse-line)))

(defn concatenate [a b]
  (let [num-digits (int (inc (math/log10 b)))]
    (+ (reduce (fn [acc _] (* acc 10)) a (range num-digits)) b)))

(defn can-be-true-part1? [acc numbers test-value]
  (cond
    (> acc test-value) false
    (and (= acc test-value) (empty? numbers)) true
    (empty? numbers) false
    :else (let [[number & remaining] numbers]
            (or (can-be-true-part1? (* acc number) remaining test-value)
                (can-be-true-part1? (+ acc number) remaining test-value)))))

(defn can-be-true-part2? [acc numbers test-value]
  (cond
    (> acc test-value) false
    (and (= acc test-value) (empty? numbers)) true
    (empty? numbers) false
    :else (let [[number & remaining] numbers]
            (or (can-be-true-part2? (* acc number) remaining test-value)
                (can-be-true-part2? (+ acc number) remaining test-value)
                (can-be-true-part2? (concatenate acc number) remaining test-value)))))

(defn calculate-sum-of-valid-equations [equations filter-fn]
  (->> (filter (fn [{:keys [test-value numbers]}] (filter-fn (first numbers) (rest numbers) test-value)) equations)
       (map :test-value)
       (apply +)))

(defn -main []
  (let [equations (parse-input)]
    (println "Part 1:" (calculate-sum-of-valid-equations equations can-be-true-part1?))
    (println "Part 2:" (calculate-sum-of-valid-equations equations can-be-true-part2?))))
