(ns day12.garden_groups
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as string]))

(defn parse-input []
  (->> (slurp (io/resource "day12.txt"))
       (string/split-lines)
       (mapv #(vec %))))

(defn all-locations [garden-map]
  (mapcat (fn [[row-idx row]]
            (map (fn [col-idx] [row-idx col-idx]) (range (count row))))
          (map-indexed vector garden-map)))

(defn get-neighbors [[row-idx col-idx]]
  [[(dec row-idx) col-idx]
   [row-idx (inc col-idx)]
   [(inc row-idx) col-idx]
   [row-idx (dec col-idx)]])

(defn get-diagonal-neighbors [[row-idx col-idx]]
  [[(dec row-idx) (inc col-idx)]
   [(inc row-idx) (inc col-idx)]
   [(inc row-idx) (dec col-idx)]
   [(dec row-idx) (dec col-idx)]])

(defn extract-region [start garden-map]
  (let [plant (get-in garden-map start)]
    (loop [queue [start]
           visited #{}]
      (if-let [current (first queue)]
        (recur (into (rest queue)
                     (filter (fn [location]
                               (and (not (contains? visited location))
                                    (= (get-in garden-map location) plant)))
                             (get-neighbors current)))
               (conj visited current))
        {:plant plant :garden-plots visited}))))

(defn extract-regions [garden-map]
  (loop [locations (all-locations garden-map)
         visited #{}
         regions []]
    (if-let [location (first locations)]
      (if (contains? visited location)
        (recur (rest locations) visited regions)
        (let [region (extract-region location garden-map)]
          (recur (rest locations) (set/union visited (region :garden-plots)) (conj regions region))))
      regions)))

(defn count-neighbors-without-plant-type [location plant garden-map]
  (->> (get-neighbors location)
       (remove #(= (get-in garden-map %) plant))
       (count)))

(defn calculate-region-area [region]
  (count (region :garden-plots)))

(defn calculate-region-perimeter [region garden-map]
  (let [plant (region :plant)]
    (->> (region :garden-plots)
         (map #(count-neighbors-without-plant-type % plant garden-map))
         (apply +))))

(defn matches-pattern [plant garden-map should-contain-plant should-not-contain-plant]
  (and (every? #(= (get-in garden-map %) plant) should-contain-plant)
       (every? #(not= (get-in garden-map %) plant) should-not-contain-plant)))

(defn count-edges [location plant garden-map]
  (let [[north east south west] (get-neighbors location)
        [northeast southeast southwest northwest] (get-diagonal-neighbors location)
        matches-pattern-pf (partial matches-pattern plant garden-map)
        checks [(matches-pattern-pf [] [north west])
                (matches-pattern-pf [] [north east])
                (matches-pattern-pf [] [south west])
                (matches-pattern-pf [] [south east])
                (matches-pattern-pf [south east] [southeast])
                (matches-pattern-pf [south west] [southwest])
                (matches-pattern-pf [north east] [northeast])
                (matches-pattern-pf [north west] [northwest])]]
    (count (filter identity checks))))

(defn calculate-region-sides [region garden-map]
  (let [plant (region :plant)]
    (->> (region :garden-plots)
         (map #(count-edges % plant garden-map))
         (apply +))))

(defn calculate-region-price-part1 [region garden-map]
  (* (calculate-region-area region) (calculate-region-perimeter region garden-map)))

(defn calculate-region-price-part2 [region garden-map]
  (* (calculate-region-area region) (calculate-region-sides region garden-map)))

(defn calculate-total-price [garden-map regions price-fn]
  (let [region-prices (map #(price-fn % garden-map) regions)]
    (apply + region-prices)))

(defn -main []
  (let [garden-map (parse-input)
        regions (extract-regions garden-map)]
    (println "Part 1:" (calculate-total-price garden-map regions calculate-region-price-part1))
    (println "Part 2:" (calculate-total-price garden-map regions calculate-region-price-part2))))
