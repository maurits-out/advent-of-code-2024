(ns day16.reindeer_maze
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.data.priority-map :refer [priority-map]]))

(defn find-start [maze]
  (some (fn [[row-idx row]]
          (some (fn [[col-idx char]]
                  (when (= char \S)
                    [row-idx col-idx]))
                (map-indexed vector row)))
        (map-indexed vector maze)))

(defn parse-input []
  (let [maze (->> (slurp (io/resource "day16.txt"))
                  (string/split-lines)
                  (mapv vec))
        start (find-start maze)]
    {:maze maze :start start}))

(defn is-end-tile? [location maze]
  (= (get-in maze location) \E))

(defn rotate-clockwise [[location direction] score]
  (let [updated-direction (case direction
                            :east :south
                            :south :west
                            :west :north
                            :north :east)]
    [[location updated-direction] (+ score 1000)]))

(defn rotate-counterclockwise [[location direction] score]
  (let [updated-direction (case direction
                            :east :north
                            :north :west
                            :west :south
                            :south :east)]
    [[location updated-direction] (+ score 1000)]))

(defn move-forward [[[row column] direction] score maze]
  (let [updated-location (case direction
                           :east [row (inc column)]
                           :north [(dec row) column]
                           :west [row (dec column)]
                           :south [(inc row) column])]
    (when (not= (get-in maze updated-location) \#)
      [[updated-location direction] (inc score)])))

(defn get-neighbors [node score maze]
  (let [clockwise (rotate-clockwise node score)
        counterclockwise (rotate-counterclockwise node score)
        forward (move-forward node score maze)]
    (if forward
      [clockwise counterclockwise forward]
      [clockwise counterclockwise])))

(defn filter-neighbors-with-lower-score [dist neighbors]
  (filter (fn [[node score]] (let [current-score (dist node)]
                               (or (not current-score) (< score current-score)))) neighbors))

(defn part1 [{:keys [maze start]}]
  (let [start-node [start :east]]
    (loop [dist {start-node 0}
           queue (priority-map start-node 0)]
      (let [[location :as node] (first (peek queue))]
        (if (is-end-tile? location maze)
          (dist node)
          (let [neighbors (->> (get-neighbors node (dist node) maze)
                               (filter-neighbors-with-lower-score dist))]
            (recur (into dist neighbors) (into (pop queue) neighbors))))))))

(defn -main []
  (let [input (parse-input)]
    (println "Part 1:" (part1 input))))
