(ns day15.warehouse_woes
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def movement-to-direction {\^ [-1 0]
                            \> [0 1]
                            \v [1 0]
                            \< [0 -1]})

(defn print-warehouse-map [warehouse-map]
  (doseq [row warehouse-map]
    (println (apply str row))))

(defn parse-warehouse [warehouse]
  (->> (string/split-lines warehouse)
       (mapv #(vec %))))

(defn parse-movements [movements]
  (->> (string/split-lines movements)
       (string/join)))

(defn parse-input []
  (let [input (slurp (io/resource "day15.txt"))
        sections (string/split input #"\n\n")]
    {:warehouse-map (parse-warehouse (first sections))
     :movements     (parse-movements (second sections))}))

(defn find-robot-location [warehouse-map]
  (some (fn [[row-idx row]]
          (some (fn [[col-idx char]]
                  (when (= char \@)
                    [row-idx col-idx]))
                (map-indexed vector row)))
        (map-indexed vector warehouse-map)))

(defn update-location [[row column] [delta-row delta-column]]
  [(+ row delta-row) (+ column delta-column)])

(defn can-move? [location direction warehouse-map]
  (let [next-location (update-location location direction)]
    (case (get-in warehouse-map next-location)
      \. true
      \# false
      \O (recur next-location direction warehouse-map))))

(defn move [location direction warehouse-map]
  (let [next-location (update-location location direction)]
    (-> (case (get-in warehouse-map next-location)
          \. warehouse-map
          \O (move next-location direction warehouse-map))
        (assoc-in next-location (get-in warehouse-map location))
        (assoc-in location \.))))

(defn apply-movement [{:keys [robot-location warehouse-map] :as state} movement]
  (let [direction (movement-to-direction movement)]
    (if (can-move? robot-location direction warehouse-map)
      {:warehouse-map  (move robot-location direction warehouse-map)
       :robot-location (update-location robot-location direction)}
      state)))

(defn gps-coordinates [warehouse-map]
  (mapcat (fn [[row-idx row]]
            (keep-indexed (fn [col-idx ch]
                            (when (= ch \O)
                              (+ (* 100 row-idx) col-idx)))
                          row))
          (map-indexed vector warehouse-map)))

(defn part1 [{:keys [warehouse-map movements]}]
  (let [robot-location (find-robot-location warehouse-map)
        initial-state {:warehouse-map warehouse-map :robot-location robot-location}
        end-state (reduce apply-movement initial-state movements)
        sum (apply + (gps-coordinates (end-state :warehouse-map)))]
    sum))

(defn -main []
  (let [input (parse-input)]
    (println "Part 1:" (part1 input))))
