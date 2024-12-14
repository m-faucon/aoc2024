(ns aoc2024.day11
  (:require
    [clojure.string :as str]
    [aoc2024.util :as util]))

(defn parse-input
  [s]
  (->> (str/split s #" ") (map parse-long)))

(defn stone-blink
  [x]
  (let [sx (str x)
        nd (count sx)]
    (cond
      (zero? x)
      [1]
      (even? nd)
      [(parse-long (subs sx 0 (/ nd 2)))
       (parse-long (subs sx (/ nd 2)))]
      :else
      [(* 2024 x)])))

(defn stones-blink
  [xs]
  (into [] (mapcat stone-blink) xs))

(-> (iterate stones-blink (parse-input (util/slurp-input)))
    (nth 25)
    count)

;; part2
;; simply replacing 25 by 75 takes too long obviously.
;; Notice we have many times the same numbers. Order doesn't matter
;; for the calculation, so we can group and record their count
;; We operate on tuples `[number count]`.
;; The `stone-blink2` function is a simple lift of the old one.
;; The `stones-blink2` function, instead of just mapcatting stone-blink2,
;; regroups them by number and aggregates the counts
;; It terminates instantly now.

(defn prepare
  [xs]
  (for [x xs] [x 1]))

(defn stone-blink2
  [[x cnt]]
  (map (fn [new-x] [new-x cnt]) (stone-blink x)))

(defn stones-blink2
  [xs]
  (reduce (fn [acc [x cnt]]
            (update acc x (fnil + 0) cnt))
          {}
          (mapcat stone-blink2 xs)))

(defn count2
  [xs]
  (->> xs (map second) (reduce +)))

(-> (iterate stones-blink2 (prepare (parse-input (util/slurp-input))))
    (nth 75)
    count2)

