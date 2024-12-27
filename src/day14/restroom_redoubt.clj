(ns day14.restroom_redoubt
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def width 101)
(def height 103)

(defn parse-line [line]
  (let [matches (re-seq #"-?\d+" line)
        [px py vx vy] (mapv #(Integer/parseInt %) matches)]
    {:p [px py] :v [vx vy]}))

(defn parse-input []
  (->> (io/resource "day14.txt")
       (slurp)
       (string/split-lines)
       (mapv parse-line)))

(defn move-robot [robot seconds]
  (let [[vx vy] (robot :v)]
    (update robot :p
            (fn [[x y]]
              [(mod (+ x (* vx seconds)) width) (mod (+ y (* vy seconds)) height)]))))

(defn move-robots [robots seconds]
  (mapv #(move-robot % seconds) robots))

(defn calculate-quadrants []
  (let [quadrant-width (quot width 2)
        quadrant-height (quot height 2)]
    [[0 0 (dec quadrant-width) (dec quadrant-height)]
     [(inc quadrant-width) 0 (dec width) (dec quadrant-height)]
     [0 (inc quadrant-height) (dec quadrant-width) (dec height)]
     [(inc quadrant-width) (inc quadrant-height) (dec width) (dec height)]]))

(defn robot-count-in-quadrant [[from-x from-y to-x to-y] robot-locations]
  (->> (map :p robot-locations)
       (filter (fn [[x y]] (and (<= from-x x to-x) (<= from-y y to-y))))
       (count)))

(defn part1 [robots]
  (let [robot-locations (move-robots robots 100)]
    (->> (calculate-quadrants)
         (map #(robot-count-in-quadrant % robot-locations))
         (apply *))))

(defn -main []
  (let [robots (parse-input)]
    (println "Part 1:" (part1 robots))))
