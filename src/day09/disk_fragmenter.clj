(ns day09.disk_fragmenter
  (:require [clojure.java.io :as io]))

(defn parse-input []
  (->> (slurp (io/resource "day09.txt"))
       (mapv #(- (int %) (int \0)))))

(defn file-system-checksum [disk-map]
  (->> (take-while identity disk-map)
       (map-indexed #(* %1 %2))
       (apply +)))

(defn input-to-disk-map [input]
  (loop [remaining input
         file-id 0
         free-space? false
         result []]
    (if-let [current (first remaining)]
      (cond
        free-space? (recur
                      (rest remaining)
                      file-id
                      false
                      (into result (repeat current nil)))
        :else (recur
                (rest remaining)
                (inc file-id)
                true
                (into result (repeat current file-id))))
      result)))

(defn part1 [input]
  (loop [disk-map (input-to-disk-map input)
         left 0
         right (dec (count disk-map))]
    (cond
      (= left right) (file-system-checksum disk-map)
      (disk-map left) (recur disk-map (inc left) right)
      (not (disk-map right)) (recur disk-map left (dec right))
      :else (let [file-id (disk-map right)]
              (recur (assoc disk-map left file-id right nil) (inc left) (dec right))))))

(defn -main []
  (let [input (parse-input)]
    (println "Part 1:" (part1 input))))
