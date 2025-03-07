(ns day23.lan_party
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.set :as set]))

(defn parse-input []
  (->> (io/resource "day23.txt")
       (slurp)
       (string/split-lines)
       (reduce (fn [acc line]
                 (let [name1 (subs line 0 2)
                       name2 (subs line 3)
                       update-fn (fnil conj #{})]
                   (-> acc
                       (update name1 update-fn name2)
                       (update name2 update-fn name1))))
               {})))

(defn connected? [network-map name1 name2]
  (contains? (network-map name1) name2))

(defn chief-historian? [name]
  (string/starts-with? name "t"))

(defn part1 [network-map]
  (count (let [names (vec (keys network-map))]
           (for [i (range (count names))
                 j (range (inc i) (count names))
                 :let [name1 (names i)
                       name2 (names j)]
                 :when (connected? network-map name1 name2)
                 k (range (inc j) (count names))
                 :let [name3 (names k)]
                 :when (and (connected? network-map name1 name3)
                            (connected? network-map name2 name3)
                            (or (chief-historian? name1)
                                (chief-historian? name2)
                                (chief-historian? name3)))]
             1))))

;; https://kaygun.github.io/clean/2019-05-06-bron-kerbosch_algorithm_in_clojure.html
(defn bron-kerbosch
  ([graph] (bron-kerbosch graph (set (keys graph)) #{} #{} #{}))
  ([graph p x r res]
   (if (and (empty? x) (empty? p))
     (set/union #{r} res)
     (loop [ps p
            xs x
            cs res]
       (if (empty? ps)
         cs
         (let [v (first ps)
               nv (graph v)]
           (recur (into #{} (rest ps))
                  (set/union #{v} xs)
                  (bron-kerbosch graph
                                 (set/intersection ps nv)
                                 (set/intersection xs nv)
                                 (set/union #{v} r)
                                 cs))))))))

(defn part2 [network-map]
  (->> (bron-kerbosch network-map)
       (apply max-key count)
       (sort)
       (string/join \,)))

(defn -main []
  (let [network-map (parse-input)]
    (println "Part 1:" (part1 network-map))
    (println "Part 2:" (part2 network-map))))
