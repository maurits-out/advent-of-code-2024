(ns day24.crossed_wires
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn parse-signals [section]
  (->> (string/split-lines section)
       (map #(string/split % #": "))
       (map (fn [[k v]] [k (Integer/parseInt v)]))
       (into {})))

(defn parse-gates [section]
  (vec (for [line (string/split-lines section)
             :let [[in1 gate in2 _ out] (string/split line #" ")]]
         {:gate (keyword (string/lower-case gate)) :in1 in1 :in2 in2 :out out})))

(defn parse-input []
  (let [input (slurp (io/resource "day24.txt"))
        [signals-section gates-section] (string/split input #"\n\n")]
    [(parse-signals signals-section) (parse-gates gates-section)]))

(defn apply-gate [gate in1 in2]
  (case gate
    :and (bit-and in1 in2)
    :or (bit-or in1 in2)
    :xor (bit-xor in1 in2)
    (throw (Exception. (str "Unknown gate: " gate)))))

(defn gate-ready? [computed-signals {:keys [in1 in2]}]
  (and (contains? computed-signals in1) (contains? computed-signals in2)))

(defn process-circuit [signals gates]
  (loop [remaining-gates gates
         computed-signals signals]
    (if (empty? remaining-gates)
      computed-signals
      (let [grouped-gates (group-by (partial gate-ready? computed-signals) remaining-gates)
            ready-gates (grouped-gates true)
            pending-gates (grouped-gates false)]
        (if (empty? ready-gates)
          (throw (Exception. "Cyclic dependency or undefined input detected!"))
          (recur pending-gates
                 (reduce (fn [sig-map {:keys [in1 in2 gate out]}]
                           (assoc sig-map out (apply-gate gate (computed-signals in1) (computed-signals in2))))
                         computed-signals
                         ready-gates)))))))

(defn calculate-z [signals]
  (->> (filter #(string/starts-with? (first %) "z") signals)
       (sort-by first)
       (reverse)
       (map second)
       (reduce #(bit-or (bit-shift-left %1 1) %2) 0)))

(defn part1 [signals gates]
  (->> (process-circuit signals gates)
       (calculate-z)))

(defn -main []
  (let [[signals gates] (parse-input)]
    (println "Part 1:" (part1 signals gates))))
