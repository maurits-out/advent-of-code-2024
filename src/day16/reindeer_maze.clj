(ns day16.reindeer_maze
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.data.priority-map :refer [priority-map]]))

(def movement-delta [[0 1] [1 0] [0 -1] [-1 0]])

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

(defn is-end-tile? [tile maze]
  (= (get-in maze tile) \E))

(defn rotate-clockwise [[tile direction]]
  (let [updated-direction (mod (inc direction) 4)]
    [tile updated-direction]))

(defn rotate-counterclockwise [[tile direction]]
  (let [updated-direction (mod (dec direction) 4)]
    [tile updated-direction]))

(defn move-forward [[[row column] direction]]
  (let [[dr dc] (nth movement-delta direction)]
    [[(+ row dr) (+ column dc)] direction]))

(defn move-backward [[[row column] direction]]
  (let [[dr dc] (nth movement-delta direction)]
    [[(- row dr) (- column dc)] direction]))

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
  (let [start-node [start-tile 0]]
    (loop [dist {start-node 0}
           queue (priority-map start-node 0)]
      (let [[tile :as node] (first (peek queue))]
        (if (is-end-tile? tile maze)
          {:dist dist :end-node node}
          (let [neighbors (->> (get-outgoing-neighbors node (dist node) maze)
                               (filter-neighbors-with-better-score dist))]
            (recur (into dist neighbors) (into (pop queue) neighbors))))))))

(defn count-tiles [end-node dist maze]
  (loop [queue [end-node]
         acc #{(first end-node)}]
    (if-let [node (first queue)]
      (let [neighbors (->> (get-incoming-neighbors node (dist node) maze)
                           (filter (fn [n] (= (dist (first n)) (second n)))))
            new-nodes (map (fn [n] (first n)) neighbors)
            tiles (map (fn [n] (first n)) new-nodes)]
        (recur (into (rest queue) new-nodes) (into acc tiles)))
      (count acc))))

(defn -main []
  (let [{:keys [maze] :as input} (parse-input)
        {:keys [dist end-node]} (dijkstra input)]
    (println "Part 1:" (dist end-node))
    (println "Part 2:" (count-tiles end-node dist maze))))
