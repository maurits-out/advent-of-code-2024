(ns day02.core
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn parse-line [line]
  (mapv #(Integer/parseInt %) (string/split line #" ")))

(defn parse-input []
  (->> (io/resource "day02.txt")
       slurp
       string/split-lines
       (mapv parse-line)))

(defn is-safe? [report]
  (loop [pairs (partition 2 1 report)
         increasing (< (first report) (second report))]
    (if (empty? pairs)
      true
      (let [pair (first pairs)
            diff (- (second pair) (first pair))]
        (and
          (= increasing (pos-int? diff))
          (not (or (zero? diff) (> diff 3) (< diff -3)))
          (recur (rest pairs) increasing))))))

(defn remove-levels [report]
  (mapv
    (fn [i] (into (subvec report 0 i) (subvec report (inc i))))
    (range 0 (count report))))

(defn is-safe-extended? [report]
  (or (is-safe? report) (some is-safe? (remove-levels report))))

(defn part1 [reports]
  (count (filter is-safe? reports)))

(defn part2 [reports]
  (count (filter is-safe-extended? reports)))

(defn -main []
  (let [reports (parse-input)]
    (println "Part 1:" (part1 reports))
    (println "Part 2:" (part2 reports))))
