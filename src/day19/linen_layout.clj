(ns day19.linen_layout
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn parse-input []
  (let [input (slurp (io/resource "day19.txt"))
        sections (string/split input #"\n\n")]
    [(string/split (first sections) #", ")
     (string/split-lines (second sections))]))

(defn is-possible? [patterns design]
  (or (empty? design)
      (some (fn [p] (and (string/starts-with? design p)
                         (is-possible? patterns (subs design (count p)))))
            patterns)))

(def count-different-ways
  (memoize (fn [patterns design]
             (if (empty? design)
               1
               (->> patterns
                    (keep (fn [p] (when (string/starts-with? design p)
                                    (count-different-ways patterns (subs design (count p))))))
                    (apply +))))))

(defn part1 [patterns designs]
  (->> designs
       (filter (partial is-possible? patterns))
       (count)))

(defn part2 [patterns designs]
  (->> designs
       (map (partial count-different-ways patterns))
       (apply +)))

(defn -main []
  (let [[patterns designs] (parse-input)]
    (println "Part 1:" (part1 patterns designs))
    (println "Part 2:" (part2 patterns designs))))
