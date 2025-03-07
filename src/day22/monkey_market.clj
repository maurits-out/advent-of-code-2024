(ns day22.monkey_market
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn parse-input []
  (->> (io/resource "day22.txt")
       (slurp)
       (string/split-lines)
       (map #(Integer/parseInt %))
       (vec)))

(defn update-secret [secret fn]
  (-> (fn secret)
      (bit-xor secret)
      (bit-and 16777215)))

(defn next-secret [secret]
  (-> secret
      (update-secret #(bit-shift-left % 6))
      (update-secret #(bit-shift-right % 5))
      (update-secret #(bit-shift-left % 11))))

(defn part1 [initial-secrets]
  (apply + (for [secret initial-secrets
                 :let [secrets-seq (iterate next-secret secret)]]
             (nth secrets-seq 2000))))

(defn first-occurrence-map [pairs]
  (reduce (fn [m [k v]]
            (if (contains? m k)
              m
              (assoc m k v)))
          {}
          pairs))

(defn calc-changes-to-price-mapping [initial-secret]
  (let [secrets (take 2000 (iterate next-secret initial-secret))
        prices (map #(rem % 10) secrets)
        price-changes (map - (rest prices) prices)]
    (->> (map vector (rest prices) price-changes)
         (partition 4 1)
         (map (fn [s] (vector (mapv second s) (first (last s)))))
         (first-occurrence-map))))

(defn part2 [initial-secrets]
  (->> (map calc-changes-to-price-mapping initial-secrets)
       (apply merge-with +)
       (vals)
       (apply max)))

(defn -main []
  (let [initial-secrets (parse-input)]
    (println "Part 1:" (part1 initial-secrets))
    (println "Part 2:" (part2 initial-secrets))))
