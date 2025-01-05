(ns day18.ram_run
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def size 70)
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
       (take 1024)
       (set)))

(defn get-neighbors [[current-x current-y] corrupted-byte-locations]
  (->> [[current-x (dec current-y)] [(inc current-x) current-y] [current-x (inc current-y)] [(dec current-x) current-y]]
       (filter (fn [[x y :as location]]
                 (and (<= 0 x size) (<= 0 y size)
                      (not (contains? corrupted-byte-locations location)))))))

(defn part1 [corrupted-byte-locations]
  (loop [queue [[start 0]]
         visited #{start}]
    (let [[location step-count] (first queue)]
      (if (= location target)
        step-count
        (let [neighbors (->> (get-neighbors location corrupted-byte-locations)
                             (filter #(not (contains? visited %))))
              new-nodes (map (fn [n] [n (inc step-count)]) neighbors)]
          (recur (into (subvec queue 1) new-nodes) (into visited neighbors)))))))

(defn -main []
  (let [corrupted-byte-locations (parse-input)]
    (println "Part 1:" (part1 corrupted-byte-locations))))
