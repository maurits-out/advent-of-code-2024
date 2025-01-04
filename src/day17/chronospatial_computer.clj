(ns day17.chronospatial_computer
  (:require [clojure.java.io :as io]
            [clojure.math :as math]
            [clojure.string :as string]))

(defn parse-input []
  (let [[registers-section program-section] (-> (io/resource "day17.txt")
                                                (slurp)
                                                (string/split #"\n\n"))]
    {:registers (let [[a b c] (->> (re-seq #"\d+" registers-section)
                                   (map #(Integer/parseInt %)))]
                  {:a a :b b :c c})
     :program   (->> (re-seq #"\d+" program-section)
                     (mapv #(Integer/parseInt %)))}))

(defn combo-value [operand registers]
  (case operand
    (0 1 2 3) operand
    4 (registers :a)
    5 (registers :b)
    6 (registers :c)))

(defn evaluate-division [operand registers]
  (->> (math/pow 2 (combo-value operand registers))
       (/ (registers :a))
       (int)))

(defn evaluate-adv [operand registers]
  (assoc registers :a (evaluate-division operand registers)))

(defn evaluate-bdv [operand registers]
  (assoc registers :b (evaluate-division operand registers)))

(defn evaluate-cdv [operand registers]
  (assoc registers :c (evaluate-division operand registers)))

(defn evaluate-bxl [operand registers]
  (assoc registers :b (bit-xor (registers :b) operand)))

(defn evaluate-bst [operand registers]
  (let [value (combo-value operand registers)
        result (mod value 8)]
    (assoc registers :b result)))

(defn evaluate-bxc [registers]
  (assoc registers :b (bit-xor (registers :b) (registers :c))))

(defn evaluate-out [operand registers]
  (mod (combo-value operand registers) 8))

(defn execute-program [{:keys [program] :as input}]
  (loop [regs (:registers input)
         ip 0
         out []]
    (if (>= ip (count program))
      (string/join "," out)
      (let [[opcode operand] (subvec program ip (+ ip 2))
            updated-regs (case opcode
                           0 (evaluate-adv operand regs)
                           1 (evaluate-bxl operand regs)
                           2 (evaluate-bst operand regs)
                           4 (evaluate-bxc regs)
                           6 (evaluate-bdv operand regs)
                           7 (evaluate-cdv operand regs)
                           regs)
            updated-ip (if (and (= opcode 3) (not (zero? (regs :a))))
                         operand
                         (+ ip 2))
            updated-out (if (= opcode 5)
                          (conj out (evaluate-out operand regs))
                          out)]
        (recur updated-regs updated-ip updated-out)))))

(defn is-correct? [a target]
  (let [b1 (mod a 8)
        b2 (bit-xor b1 5)
        b3 (bit-xor b2 6)
        c (bit-shift-right a b2)
        b4 (bit-xor b3 c)]
    (= (mod b4 8) target)))

(defn find-initial-value [program idx current-a]
  (if (neg? idx)
    current-a
    (some (fn [n] (let [updated-a (bit-or (bit-shift-left current-a 3) n)
                        target (nth program idx)]
                    (and (is-correct? updated-a target)
                         (find-initial-value program (dec idx) updated-a)))) (range 8))))

(defn part1 [input]
  (execute-program input))

(defn part2 [program]
  (find-initial-value program (dec (count program)) 0))

(defn -main []
  (let [input (parse-input)]
    (println "Part 1:" (part1 input))
    (println "Part 2:" (part2 (input :program)))))
