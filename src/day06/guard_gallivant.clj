(ns day06.guard-gallivant
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn find-guard-position [puzzle-map]
  (some (fn [[row-idx row]]
          (some (fn [[col-idx char]]
                  (when (= char \^)
                    [row-idx col-idx]))
                (map-indexed vector row)))
        (map-indexed vector puzzle-map)))

(defn parse-input []
  (let [input (slurp (io/resource "day06.txt"))
        lines (string/split-lines input)
        puzzle-map (mapv #(vec %) lines)
        guard-position (find-guard-position puzzle-map)]
    {:puzzle-map puzzle-map :guard-position guard-position}))

(defn move [[row column] direction]
  (case direction
    :up [(dec row) column]
    :down [(inc row) column]
    :left [row (dec column)]
    :right [row (inc column)]))

(defn turn-right [direction]
  (case direction
    :up :right
    :right :down
    :down :left
    :left :up))

(defn part1 [{:keys [puzzle-map guard-position]}]
  (loop [direction :up
         location guard-position
         visited #{guard-position}]
    (let [next-location (move location direction)]
      (case (get-in puzzle-map next-location)
        nil (count visited)
        \# (recur (turn-right direction) location visited)
        (recur direction next-location (conj visited next-location))))))

(defn candidate-locations [puzzle-map]
  (mapcat
    (fn [[row-idx row]]
      (keep-indexed
        (fn [col-idx char]
          (when (= char \.)
            [row-idx col-idx]))
        row))
    (map-indexed vector puzzle-map)))

(defn next-state [puzzle-map [location direction]]
  (let [next-location (move location direction)]
    (if (= (get-in puzzle-map next-location) \#)
      [location (turn-right direction)]
      [next-location direction])))

(defn is-loop? [puzzle-map guard-position]
  (loop [state [guard-position :up]
         visited #{}]
    (cond
      (visited state) true
      (not (get-in puzzle-map (first state))) false
      :else (recur (next-state puzzle-map state) (conj visited state)))))

(defn part2 [{:keys [puzzle-map guard-position]}]
  (->> (candidate-locations puzzle-map)
       (map #(assoc-in puzzle-map % \#))
       (filter #(is-loop? % guard-position))
       (count)))

(defn -main []
  (let [input (parse-input)]
    (println "Part 1:" (part1 input))
    (println "Part 2:" (part2 input))))
