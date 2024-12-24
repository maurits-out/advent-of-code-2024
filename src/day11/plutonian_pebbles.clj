(ns day11.plutonian_pebbles
  (:require [clojure.math :as math]))

(def input [5178527 8525 22 376299 3 69312 0 275])

(defn split [num num-digits]
  (let [div (int (math/pow 10 num-digits))]
    [(quot num div) (rem num div)]))

(defn digit-count [num]
  (int (inc (math/floor (math/log10 num)))))

(def stone-count
  (memoize (fn [stone times]
             (cond
               (zero? times) 1
               (zero? stone) (stone-count 1 (dec times))
               :else (let [num-digits (digit-count stone)]
                       (if (odd? num-digits)
                         (stone-count (* stone 2024) (dec times))
                         (->> (split stone (quot num-digits 2))
                              (map #(stone-count % (dec times)))
                              (apply +))))))))

(defn blink [times]
  (->> (map #(stone-count % times) input)
       (apply +)))

(defn -main []
  (println "Part 1:" (blink 25))
  (println "Part 2:" (blink 75)))
