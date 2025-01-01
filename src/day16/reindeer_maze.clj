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

(defn rotate-clockwise [{:keys [direction] :as node}]
  (assoc node :direction (mod (inc direction) 4)))

(defn rotate-counterclockwise [{:keys [direction] :as node}]
  (assoc node :direction (mod (dec direction) 4)))

(defn move-forward [{:keys [tile direction] :as node}]
  (let [[dr dc] (nth movement-delta direction)]
    (assoc node :tile [(+ (first tile) dr) (+ (second tile) dc)])))

(defn move-backward [{:keys [tile direction] :as node}]
  (let [[dr dc] (nth movement-delta direction)]
    (assoc node :tile [(- (first tile) dr) (- (second tile) dc)])))

(defn get-outgoing-neighbors-with-score [node score maze]
  (let [clockwise [(rotate-clockwise node) (+ score 1000)]
        counterclockwise [(rotate-counterclockwise node) (+ score 1000)]
        forward [(move-forward node) (inc score)]]
    (if (not= (get-in maze (:tile (first forward))) \#)
      [clockwise counterclockwise forward]
      [clockwise counterclockwise])))

(defn get-incoming-neighbors-with-score [node score maze]
  (let [clockwise [(rotate-clockwise node) (- score 1000)]
        counterclockwise [(rotate-counterclockwise node) (- score 1000)]
        backward [(move-backward node) (dec score)]]
    (if (not= (get-in maze (:tile (first backward))) \#)
      [clockwise counterclockwise backward]
      [clockwise counterclockwise])))

(defn filter-neighbors-with-better-score [dist neighbors]
  (filter (fn [[node score]] (let [current-score (dist node)]
                               (or (not current-score) (< score current-score)))) neighbors))

(defn dijkstra [{:keys [maze start-tile]}]
  (let [start-node {:tile start-tile :direction 0}]
    (loop [dist {start-node 0}
           queue (priority-map start-node 0)]
      (let [{:keys [tile] :as node} (first (peek queue))]
        (if (is-end-tile? tile maze)
          {:dist dist :end-node node}
          (let [neighbors (->> (get-outgoing-neighbors-with-score node (dist node) maze)
                               (filter-neighbors-with-better-score dist))]
            (recur (into dist neighbors) (into (pop queue) neighbors))))))))

(defn count-tiles [end-node dist maze]
  (loop [queue [end-node]
         acc #{(:tile end-node)}]
    (if-let [node (first queue)]
      (let [neighbors (->> (get-incoming-neighbors-with-score node (dist node) maze)
                           (filter (fn [n] (= (dist (first n)) (second n)))))
            new-nodes (map #(first %) neighbors)
            tiles (map #(:tile %) new-nodes)]
        (recur (into (rest queue) new-nodes) (into acc tiles)))
      (count acc))))

(defn -main []
  (let [{:keys [maze] :as input} (parse-input)
        {:keys [dist end-node]} (dijkstra input)]
    (println "Part 1:" (dist end-node))
    (println "Part 2:" (count-tiles end-node dist maze))))
