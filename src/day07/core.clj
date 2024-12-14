(ns day07.core
  (:require [clojure.java.io :as io]
            [clojure.math :as math]
            [clojure.string :as string]))

(defn parse-line [line]
  (let [parts (string/split line #":? ")
        numbers (map #(parse-long %) parts)]
    {:test-value (first numbers) :numbers (vec (rest numbers))}))

(defn parse-input []
  (->> (slurp (io/resource "day07.txt"))
       (string/split-lines)
       (mapv parse-line)))

(defn concatenate [a b]
  (let [num-digits (int (inc (math/log10 b)))]
    (+ (* a (int (math/pow 10 num-digits))) b)))

(defn can-be-true? [acc numbers test-value ops]
  (cond
    (> acc test-value) false
    (and (= acc test-value) (empty? numbers)) true
    (empty? numbers) false
    :else (let [[number & remaining] numbers]
            (first (filter identity (map
                                      (fn [op] (can-be-true? (op acc number) remaining test-value ops))
                                      ops))))))

(defn sum-of-valid-equations [equations ops]
  (->> (filter (fn [{:keys [test-value numbers]}] (can-be-true? (first numbers) (rest numbers) test-value ops)) equations)
       (map :test-value)
       (apply +)))

(defn -main []
  (let [equations (parse-input)]
    (println "Part 1:" (sum-of-valid-equations equations [+ *]))
    (println "Part 2:" (sum-of-valid-equations equations [+ * concatenate]))))
