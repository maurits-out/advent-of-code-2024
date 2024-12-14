(ns day08.resonant_collinearity
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn extract-antennas [lines]
  (->> (mapcat
         (fn [[row-idx row]]
           (keep-indexed
             (fn [col-idx char]
               (when (not= char \.)
                 [char [row-idx col-idx]]))
             row))
         (map-indexed vector lines))
       (group-by first)
       (map (fn [[k v]] [k (mapv second v)]))
       (into {})))

(defn parse-input []
  (let [input (slurp (io/resource "day08.txt"))
        lines (string/split-lines input)]
    {:height (count lines) :width (count (first lines)) :antennas (extract-antennas lines)}))

(defn is-within-bounds? [[row column] height width]
  (and (<= 0 row (dec height)) (<= 0 column (dec width))))

(defn anti-node-from-antennas [[row1 col1] [row2 col2] height width]
  (let [anti-node [(- (* 2 row2) row1) (- (* 2 col2) col1)]]
    (when (is-within-bounds? anti-node height width)
      anti-node)))

(defn anti-nodes [coordinates height width]
  (->> (for [from coordinates
             to coordinates
             :when (not= from to)]
         (anti-node-from-antennas from to height width))
       (filter identity)))

(defn part1 [{:keys [height width antennas]}]
  (->> (mapcat
         #(anti-nodes % height width)
         (map #(second %) antennas))
       distinct
       count))

(defn -main []
  (let [input (parse-input)]
    (println "Part 1:" (part1 input))))
