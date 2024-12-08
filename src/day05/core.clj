(ns day05.core
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.set :as set]))

(defn to-page-number-set [rules]
  (into #{} (map #(Integer/parseInt (subs % 3)) rules)))

(defn parse-rules-section [section]
  (->> (string/split-lines section)
       (group-by #(Integer/parseInt (subs % 0 2)))
       (into {} (map (fn [[k v]] [k (to-page-number-set v)])))))

(defn parse-csv-line [line]
  (->> (string/split line #",")
       (map #(Integer/parseInt %))))

(defn parse-updates-section [section]
  (let [lines (string/split-lines section)]
    (mapv parse-csv-line lines)))

(defn parse-input []
  (let [input (slurp (io/resource "day05.txt"))
        sections (string/split input #"\n\n")]
    {:rules   (parse-rules-section (first sections))
     :updates (parse-updates-section (last sections))}))

(defn middle-page-number [update]
  (let [index (quot (count update) 2)]
    (nth update index)))

(defn is-correctly-ordered? [rules page-numbers]
  (if-let [[current-page & remaining] page-numbers]
    (if (set/subset? (set remaining) (rules current-page))
      (recur rules remaining)
      false)
    true))

(defn part1 [{:keys [rules updates]}]
  (->> (filter (partial is-correctly-ordered? rules) updates)
       (reduce #(+ %1 (middle-page-number %2)) 0)))

(defn fix-update [rules incorrect-update]
  (let [page-numbers-in-update (set incorrect-update)
        page-number-page-count-after (map (fn [page-number]
                                            (let [common-pages (set/intersection (rules page-number) page-numbers-in-update)]
                                              [page-number (count common-pages)])) incorrect-update)
        ordered-by-count-desc (sort-by (comp - second) page-number-page-count-after)]
    (map first ordered-by-count-desc)))

(defn part2 [{:keys [rules updates]}]
  (->> (filter #(not (is-correctly-ordered? rules %)) updates)
       (map #(fix-update rules %))
       (map #(middle-page-number %))
       (apply +)))

(defn -main []
  (let [input (parse-input)]
    (println "Part 1:" (part1 input))
    (println "Part 2:" (part2 input))))

