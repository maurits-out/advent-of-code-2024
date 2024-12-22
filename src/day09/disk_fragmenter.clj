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
      (if free-space?
        (recur (rest remaining) file-id false (into result (repeat current nil)))
        (recur (rest remaining) (inc file-id) true (into result (repeat current file-id))))
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

(defn extract-blocks [input]
  (loop [remaining input
         offset 0
         file-id 0
         free-space-block? false
         files []
         free-space-blocks []]
    (if-let [size (first remaining)]
      (if free-space-block?
        (recur (rest remaining) (+ offset size) file-id false files
               (conj free-space-blocks {:offset offset :size size}))
        (recur (rest remaining) (+ offset size) (inc file-id) true
               (conj files {:offset offset :size size :file-id file-id}) free-space-blocks))
      {:files (vec (reverse files)) :free-space-blocks free-space-blocks})))

(defn get-free-space-block-idx [requested-size free-space-blocks start end]
  (when (< start end)
    (->> (subvec free-space-blocks start end)
         (map-indexed (fn [idx block] [(+ start idx) (block :size)]))
         (some (fn [[idx size]]
                 (when (>= size requested-size)
                   idx))))))

(defn checksum-for-file [{:keys [file-id size offset]}]
  (quot (* file-id size (+ (* 2 offset) (dec size))) 2))

(defn part2 [input]
  (let [blocks (extract-blocks input)]
    (loop [files (blocks :files)
           checksum 0
           free-space-blocks (blocks :free-space-blocks)
           start-at (vec (repeat 10 0))]
      (if-let [file (first files)]
        (if-let [idx (get-free-space-block-idx (file :size) free-space-blocks (start-at (file :size)) (file :file-id))]
          (recur
            (rest files)
            (+ checksum (checksum-for-file (assoc file :offset (get-in free-space-blocks [idx :offset]))))
            (update free-space-blocks idx (fn [block] (-> block
                                                          (update :size #(- % (file :size)))
                                                          (update :offset #(+ % (file :size))))))
            (assoc start-at (file :size) idx))
          (recur
            (rest files)
            (+ checksum (checksum-for-file file))
            free-space-blocks
            (assoc start-at (file :size) (count free-space-blocks))))
        checksum))))

(defn -main []
  (let [input (parse-input)]
    (println "Part 1:" (part1 input))
    (println "Part 2:" (part2 input))))
