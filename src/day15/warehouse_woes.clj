(ns day15.warehouse_woes
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def movement-to-direction {\^ [-1 0]
                            \> [0 1]
                            \v [1 0]
                            \< [0 -1]})

(defn parse-warehouse [warehouse]
  (->> (string/split-lines warehouse)
       (mapv #(vec %))))

(defn parse-movements [movements]
  (->> (string/split-lines movements)
       (string/join)))

(defn parse-input []
  (let [input (slurp (io/resource "day15.txt"))
        sections (string/split input #"\n\n")]
    {:warehouse (parse-warehouse (first sections))
     :movements (parse-movements (second sections))}))

(defn find-robot [warehouse-map]
  (some (fn [[row-idx row]]
          (some (fn [[col-idx char]]
                  (when (= char \@)
                    [row-idx col-idx]))
                (map-indexed vector row)))
        (map-indexed vector warehouse-map)))

(defn update-location [[row column] [delta-row delta-column]]
  [(+ row delta-row) (+ column delta-column)])

(defn is-direction-horizontal? [direction]
  (zero? (first direction)))

(defn box-locations [[row column :as location] warehouse]
  (case (get-in warehouse location)
    \[ [location [row (inc column)]]
    \] [location [row (dec column)]]))

(defn can-move? [location direction warehouse]
  (let [next-location (update-location location direction)]
    (case (get-in warehouse next-location)
      \. true
      \# false
      \O (can-move? next-location direction warehouse)
      (\[ \]) (if (is-direction-horizontal? direction)
                (can-move? next-location direction warehouse)
                (every? #(can-move? % direction warehouse)
                        (box-locations next-location warehouse))))))

(defn move [location direction warehouse]
  (let [next-location (update-location location direction)]
    (-> (case (get-in warehouse next-location)
          \. warehouse
          \O (move next-location direction warehouse)
          (\[ \]) (if (is-direction-horizontal? direction)
                    (move next-location direction warehouse)
                    (->> (box-locations next-location warehouse)
                         (reduce (fn [w l] (move l direction w)) warehouse))))
        (assoc-in next-location (get-in warehouse location))
        (assoc-in location \.))))

(defn apply-movement [{:keys [robot warehouse] :as state} movement]
  (let [direction (movement-to-direction movement)]
    (if (can-move? robot direction warehouse)
      {:warehouse (move robot direction warehouse)
       :robot     (update-location robot direction)}
      state)))

(defn gps-coordinates [warehouse]
  (mapcat (fn [[row-idx row]]
            (keep-indexed (fn [col-idx ch]
                            (when (or (= ch \O) (= ch \[))
                              (+ (* 100 row-idx) col-idx)))
                          row))
          (map-indexed vector warehouse)))

(defn scale-up-row [row]
  (vec (mapcat (fn [ch] (case ch
                          \# [\# \#]
                          \O [\[ \]]
                          \. [\. \.]
                          \@ [\@ \.]))
               row)))

(defn scale-up [warehouse]
  (mapv scale-up-row warehouse))

(defn solve [{:keys [warehouse movements]}]
  (let [robot (find-robot warehouse)
        initial-state {:warehouse warehouse :robot robot}
        end-state (reduce apply-movement initial-state movements)]
    (apply + (gps-coordinates (end-state :warehouse)))))

(defn part1 [input]
  (solve input))

(defn part2 [input]
  (->> (update input :warehouse scale-up)
       (solve)))

(defn -main []
  (let [input (parse-input)]
    (println "Part 1:" (part1 input))
    (println "Part 2:" (part2 input))))
