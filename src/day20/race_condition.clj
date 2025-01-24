(ns day20.race_condition
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def steps [[-1 0] [0 1] [1 0] [0 -1]])

(defn find-start-track [maze]
  (some (fn [[row-idx row]]
          (some (fn [[col-idx char]]
                  (when (= char \S)
                    [row-idx col-idx]))
                (map-indexed vector row)))
        (map-indexed vector maze)))

(defn parse-input []
  (let [race-track-map (->> (slurp (io/resource "day20.txt"))
                            (string/split-lines)
                            (mapv vec))
        start-track (find-start-track race-track-map)]
    [race-track-map start-track]))

(defn add [[row column] [dr dc]]
  [(+ row dr) (+ column dc)])

(defn get-neighbors [race-track-map position]
  (->> steps
       (map #(add position %))
       (filter (fn [l] (contains? #{\. \E} (get-in race-track-map l))))))

(defn find-shortest-course [race-track-map start-track]
  (loop [queue [(list start-track)]
         visited #{start-track}]
    (if-let [[position :as course] (first queue)]
      (if (= (get-in race-track-map position) \E)
        (vec (reverse course))
        (let [neighbors (->> (get-neighbors race-track-map position)
                             (remove #(contains? visited %)))
              updated-courses (map #(conj course %) neighbors)]
          (recur (into (subvec queue 1) updated-courses)
                 (into visited neighbors)))))))

(defn manhattan-distance [[r1 c1] [r2 c2]]
  (+ (abs (- r2 r1)) (abs (- c2 c1))))

(defn count-cheats [shortest-course max-picoseconds]
  (count (for [i (range (dec (count shortest-course)))
               j (range (inc i) (count shortest-course))
               :let [dist (manhattan-distance (nth shortest-course i) (nth shortest-course j))]
               :when (and (>= (- j i dist) 100) (<= dist max-picoseconds))]
           1)))

(defn -main []
  (let [[race-track-map start-track] (parse-input)
        shortest-course (find-shortest-course race-track-map start-track)]
    (println "Part 1:" (count-cheats shortest-course 2))
    (println "Part 2:" (count-cheats shortest-course 20))))
