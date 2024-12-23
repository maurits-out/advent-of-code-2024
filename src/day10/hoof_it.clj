(ns day10.hoof_it
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.set :as set]))

(defn parse-line [line]
  (mapv #(- (int %) (int \0)) line))

(defn parse-input []
  (->> (slurp (io/resource "day10.txt"))
       (string/split-lines)
       (mapv parse-line)))

(defn find-lowest-locations [topographic-map]
  (->> (mapcat (fn [[row-idx row]]
                 (keep-indexed (fn [col-idx height]
                                 (when (= height 0) [row-idx col-idx]))
                               row))
               (map-indexed vector topographic-map))
       (vec)))

(defn get-next-locations [topographic-map [row-idx col-idx] height]
  (->> [[(dec row-idx) col-idx]
        [row-idx (inc col-idx)]
        [(inc row-idx) col-idx]
        [row-idx (dec col-idx)]]
       (filter #(= (get-in topographic-map %) height))))

(defn get-reachable-9-height-locations [topographic-map current]
  (let [height (get-in topographic-map current)]
    (if (= height 9)
      #{current}
      (->> (get-next-locations topographic-map current (inc height))
           (map #(get-reachable-9-height-locations topographic-map %))
           (apply set/union)))))

(defn part1 [topographic-map start-locations]
  (->> (map #(get-reachable-9-height-locations topographic-map %) start-locations)
       (map count)
       (apply +)))

(defn count-distinct-hiking-trails [topographic-map location]
  (let [height (get-in topographic-map location)]
    (if (= height 9)
      1
      (->> (get-next-locations topographic-map location (inc height))
           (map #(count-distinct-hiking-trails topographic-map %))
           (apply +)))))

(defn part2 [topographic-map start-locations]
  (->> start-locations
       (map #(count-distinct-hiking-trails topographic-map %))
       (apply +)))

(defn -main []
  (let [topographic-map (parse-input)
        start-locations (find-lowest-locations topographic-map)]
    (println "Part 1:" (part1 topographic-map start-locations))
    (println "Part 2:" (part2 topographic-map start-locations))))
