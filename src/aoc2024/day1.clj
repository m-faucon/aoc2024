(ns aoc2024.day1
  (:require [aoc2024.util :as util]
            [clojure.string :as str]))

(defn input
  []
  (->> (util/input-lines)
       (reduce (fn [[left right] line]
                 (let [[l r] (str/split line #"\s+")]
                   [(conj left (parse-long l))
                    (conj right (parse-long r))]))
               [[] []])))

(let [[left right] (input)
      left (sort left)
      right (sort right)]
  (->> (map (fn [a b] (Math/abs (- b a)))
            left right)
       (reduce +)))

;; part2

;; NOTE: would be more efficient to iterate the second one
;; rather than fetching all distinct numbers in a set,
;; but this shows the operation is symmetrical

(let [[left right]
      (input)
      left-freqs (frequencies left)
      right-freqs (frequencies right)
      distinct-keys (into #{} (concat left right))]
  (->> distinct-keys
       (map (fn [n] (* (left-freqs n 0)
                       (right-freqs n 0)
                       n)))
       (reduce +)))
