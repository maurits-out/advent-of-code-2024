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

(defn follow-trail-part1 [topographic-map current]
  (let [height (get-in topographic-map current)]
    (if (= height 9)
      #{current}
      (let [locations (get-next-locations topographic-map current (inc height))]
        (apply set/union
               (map #(follow-trail-part1 topographic-map %) locations))))))

(defn part1 [topographic-map start-locations]
  (->> (map #(follow-trail-part1 topographic-map %) start-locations)
       (map count)
       (apply +)))

(defn follow-trail-part2 [topographic-map path]
  (let [current (first path)
        height (get-in topographic-map current)]
    (if (= height 9)
      [path]
      (let [locations (get-next-locations topographic-map current (inc height))]
        (mapcat identity (mapv #(follow-trail-part2 topographic-map (conj path %)) locations))))))

(defn part2 [topographic-map start-locations]
  (->> (map #(follow-trail-part2 topographic-map (list %)) start-locations)
       (map count)
       (apply +)))

(defn -main []
  (let [topographic-map (parse-input)
        start-locations (find-lowest-locations topographic-map)]
    (println "Part 1:" (part1 topographic-map start-locations))
    (println "Part 2:" (part2 topographic-map start-locations))))
