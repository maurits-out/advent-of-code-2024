(ns day08.resonant_collinearity
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn extract-antenna-locations [lines]
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
    {:height (count lines) :width (count (first lines)) :antennas (extract-antenna-locations lines)}))

(defn is-within-bounds? [[row column] height width]
  (and (<= 0 row (dec height)) (<= 0 column (dec width))))

(defn anti-nodes-for-antennas [from-antenna to-antenna height width]
  (let [row-step (- (first to-antenna) (first from-antenna))
        column-step (- (second to-antenna) (second from-antenna))]
    (take-while #(is-within-bounds? % height width)
                (iterate (fn [[row-idx col-idx]] [(+ row-idx row-step) (+ col-idx column-step)]) to-antenna))))

(defn two-anti-nodes [antenna-locations height width]
  (->> (for [from antenna-locations
             to antenna-locations
             :when (not= from to)]
         (second (anti-nodes-for-antennas from to height width)))
       (filter identity)))

(defn all-anti-nodes [antenna-locations height width]
  (->> (for [from antenna-locations
             to antenna-locations
             :when (not= from to)]
         (anti-nodes-for-antennas from to height width))
       (mapcat identity)
       (filter identity)))

(defn part1 [{:keys [height width antennas]}]
  (->> (mapcat
         #(two-anti-nodes % height width)
         (map #(second %) antennas))
       distinct
       count))

(defn part2 [{:keys [height width antennas]}]
  (->> (mapcat
         #(all-anti-nodes % height width)
         (map #(second %) antennas))
       distinct
       count))

(defn -main []
  (let [input (parse-input)]
    (println "Part 1:" (part1 input))
    (println "Part 2:" (part2 input))))
