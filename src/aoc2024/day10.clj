(ns aoc2024.day10
  (:require
    [aoc2024.util :as util]
    [clojure.string :as str]))

(defn parse-input
  [s]
  (let [lines (str/split-lines s)
        carte (->> lines
                   (map-indexed
                    (fn [y line]
                      (map-indexed
                       (fn [x c]
                         [[x y] (-> c str parse-long)])
                       line)))
                   (into {} cat))
        ]
    {:carte carte
     :W (count (first lines))
     :H (count lines)
     :trailheads (->> carte
                      (filter (comp #{0} val))
                      (map first))}))

(defn leaf?
  [carte coord]
  (= 9 (carte coord)))

(let [{:keys [carte W H trailheads]} (parse-input (util/slurp-input))
      leaf? (partial leaf? carte)
      children (fn [coord]
                 (->> (util/man-neighbours coord)
                      (filter (every-pred (partial util/inside? W H)
                                          (fn [new-coord]
                                            (= (inc (carte coord))
                                               (carte new-coord)))))))
      trailhead-score (fn [trailhead]
                        (->> (tree-seq (complement leaf?) children trailhead)
                             (filter leaf?)
                             (into #{})
                             count))]
  (->> (map trailhead-score trailheads)
       (reduce +)))

;; part2

;; copy paste part1 and remove a single line :
;; - (into #{})
;; (and renaming)

(let [{:keys [carte W H trailheads]} (parse-input (util/slurp-input))
      leaf? (partial leaf? carte)
      children (fn [coord]
                 (->> (util/man-neighbours coord)
                      (filter (every-pred (partial util/inside? W H)
                                          (fn [new-coord]
                                            (= (inc (carte coord))
                                               (carte new-coord)))))))
      trailhead-rating (fn [trailhead]
                         (->> (tree-seq (complement leaf?) children trailhead)
                              (filter leaf?)
                              count))]
  (->> (map trailhead-rating trailheads)
       (reduce +)))
