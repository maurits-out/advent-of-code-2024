(ns day18.ram_run
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def size 70)
(def size-first-batch 1024)
(def start [0 0])
(def target [size size])

(defn parse-line [line]
  (->> (string/split line #",")
       (mapv #(Integer/parseInt %))))

(defn parse-input []
  (->> (io/resource "day18.txt")
       (slurp)
       (string/split-lines)
       (map parse-line)
       (split-at size-first-batch)))

(defn get-neighbors [[current-x current-y] corrupted-byte-locations]
  (->> [[current-x (dec current-y)]
        [(inc current-x) current-y]
        [current-x (inc current-y)]
        [(dec current-x) current-y]]
       (filter (fn [[x y :as location]]
                 (and (<= 0 x size) (<= 0 y size)
                      (not (contains? corrupted-byte-locations location)))))))

(defn find-shortest-path-locations [corrupted-byte-locations]
  (loop [queue [(list start)]
         visited #{start}]
    (if-let [[location :as path] (first queue)]
      (if (= location target)
        (set path)
        (let [neighbors (->> (get-neighbors location corrupted-byte-locations)
                             (filter #(not (contains? visited %))))
              updated-paths (map #(conj path %) neighbors)]
          (recur (into (subvec queue 1) updated-paths)
                 (into visited neighbors)))))))

(defn first-byte-to-block-exit [shortest-path-locations corrupted-byte-locations remaining]
  (let [next-corrupted-location (first remaining)
        new-corrupted-byte-locations (conj corrupted-byte-locations next-corrupted-location)]
    (if (contains? shortest-path-locations next-corrupted-location)
      (if-let [new-shortest-path-locations (find-shortest-path-locations new-corrupted-byte-locations)]
        (recur new-shortest-path-locations new-corrupted-byte-locations (rest remaining))
        (string/join "," next-corrupted-location))
      (recur shortest-path-locations new-corrupted-byte-locations (rest remaining)))))

(defn -main []
  (let [[first-batch second-batch] (parse-input)
        shortest-path (find-shortest-path-locations (set first-batch))]
    (println "Part 1:" (dec (count shortest-path)))
    (println "Part 2:" (first-byte-to-block-exit shortest-path (set first-batch) second-batch))))
