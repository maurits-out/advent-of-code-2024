(ns day19.linen_layout
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn parse-input []
  (let [input (slurp (io/resource "day19.txt"))
        sections (string/split input #"\n\n")]
    {:patterns (string/split (first sections) #", ")
     :designs  (string/split-lines (second sections))}))

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

(defn part1 [{:keys [patterns designs]}]
  (->> designs
       (filter (partial is-possible? patterns))
       (count)))

(defn part2 [{:keys [patterns designs]}]
  (->> designs
       (map (partial count-different-ways patterns))
       (apply +)))

(defn -main []
  (let [input (parse-input)]
    (println "Part 1:" (part1 input))
    (println "Part 2:" (part2 input))))
