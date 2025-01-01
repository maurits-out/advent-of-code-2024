(ns day16.reindeer_maze
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.data.priority-map :refer [priority-map]]))

(def directions [[0 1] [1 0] [0 -1] [-1 0]])

(defn find-start-tile [maze]
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
        start (find-start-tile maze)]
    {:maze maze :start-tile start}))

(defn is-end-tile? [location maze]
  (= (get-in maze location) \E))

(defn rotate-clockwise [[location direction]]
  (let [updated-direction (case direction
                            :east :south
                            :south :west
                            :west :north
                            :north :east)]
    [location updated-direction]))

(defn rotate-counterclockwise [[location direction]]
  (let [updated-direction (case direction
                            :east :north
                            :north :west
                            :west :south
                            :south :east)]
    [location updated-direction]))

(defn move-forward [[[row column] direction]]
  (let [updated-location (case direction
                           :east [row (inc column)]
                           :north [(dec row) column]
                           :west [row (dec column)]
                           :south [(inc row) column])]
    [updated-location direction]))

(defn move-backward [[[row column] direction]]
  (let [updated-location (case direction
                           :east [row (dec column)]
                           :north [(inc row) column]
                           :west [row (inc column)]
                           :south [(dec row) column])]
    [updated-location direction]))

(defn get-outgoing-neighbors [node score maze]
  (let [clockwise [(rotate-clockwise node) (+ score 1000)]
        counterclockwise [(rotate-counterclockwise node) (+ score 1000)]
        forward [(move-forward node) (inc score)]]
    (if (not= (get-in maze (ffirst forward)) \#)
      [clockwise counterclockwise forward]
      [clockwise counterclockwise])))

(defn get-incoming-neighbors [node score maze]
  (let [clockwise [(rotate-clockwise node) (- score 1000)]
        counterclockwise [(rotate-counterclockwise node) (- score 1000)]
        backward [(move-backward node) (dec score)]]
    (if (not= (get-in maze (ffirst backward)) \#)
      [clockwise counterclockwise backward]
      [clockwise counterclockwise])))

(defn filter-neighbors-with-better-score [dist neighbors]
  (filter (fn [[node score]] (let [current-score (dist node)]
                               (or (not current-score) (< score current-score)))) neighbors))

(defn dijkstra [{:keys [maze start-tile]}]
  (let [start-node [start-tile :east]]
    (loop [dist {start-node 0}
           queue (priority-map start-node 0)]
      (let [[location :as node] (first (peek queue))]
        (if (is-end-tile? location maze)
          {:dist dist :end-node node}
          (let [neighbors (->> (get-outgoing-neighbors node (dist node) maze)
                               (filter-neighbors-with-better-score dist))]
            (recur (into dist neighbors) (into (pop queue) neighbors))))))))

(defn count-tiles [end-node dist maze]
  (loop [queue [end-node]
         tiles #{(first end-node)}]
    (if-let [node (first queue)]
      (let [neighbors (->> (get-incoming-neighbors node (dist node) maze)
                           (filter (fn [n] (= (dist (first n)) (second n)))))
            new-nodes (map (fn [n] (first n)) neighbors)
            locations (map (fn [n] (first n)) new-nodes)]
        (recur (into (rest queue) new-nodes) (into tiles locations)))
      (count tiles))))

(defn -main []
  (let [{:keys [maze] :as input} (parse-input)
        {:keys [dist end-node]} (dijkstra input)]
    (println "Part 1:" (dist end-node))
    (println "Part 2:" (count-tiles end-node dist maze))))
