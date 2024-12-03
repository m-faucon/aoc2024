(ns aoc2024.day2
  (:require
    [aoc2024.util :as util]
    [clojure.string :as str]))

(defn input
  []
  (->> (util/input-lines)
       (map (fn [l] (->> (str/split l #" ") (map parse-long))))))

(defn safe-line?
  [xs]
  (let [diffs (map - xs (rest xs))]
    (or (every? #(<= 1 % 3) diffs)
        (every? #(<= -3 % -1) diffs))))

(->> (input)
     (filter safe-line?)
     count)

;; part2

(->> (input) (map count) (apply max))
;; => 8
;; 8 * instant = instant so I'm not going to bother
;; optimizing this. just try to remove each level
;; until something works

(defn all-with-one-removed
  [xs]
  (let [indexed (map-indexed vector xs)]
    (for [i (map first indexed)]
      (->> indexed
           (remove #(= i (first %)))
           (map second)))))

(defn almost-safe-line?
  [xs]
  (some safe-line? (all-with-one-removed xs)))

(->> (input)
     (filter almost-safe-line?)
     count)
