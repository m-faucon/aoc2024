(ns aoc2024.day6
  (:require [aoc2024.util :as util]
            [clojure.string :as str]))

(defn find-start-coord
  [carte]
  (->> carte
       (map-indexed (fn [y line]
                      (map-indexed (fn [x tile] [[x y] tile])
                                   line)))
       (sequence cat)
       (some (fn [[coord tile]] (when (= \^ tile) coord)))))

(defn parse-input
  [s]
  (let [carte (->> (str/split-lines s)
                   (mapv vec))]
    {:carte carte
     :start-coord (find-start-coord carte)}))

;; aoc tells us this will terminate, so we don't have
;; to worry about it.

(defn compute-visited-coords
  [{:keys [carte start-coord]}]
  (loop [coord start-coord
         dir util/up
         visited #{}]
    (let [new-coord (util/v+ coord dir)
          new-tile (util/xy-or-nil carte new-coord)]
      (case new-tile
        nil (conj visited coord)
        \# (recur coord (util/rotate-90-clockwise dir) visited)
        (recur new-coord dir (conj visited coord))))))

;; part2

(defn will-loop?
  [{:keys [carte start-coord]}]
  (loop [coord start-coord
         dir util/up
         ;; now visited is #{[coord dir] ...}
         visited #{}]
    (or (visited [coord dir])
        (let [new-coord (util/v+ coord dir)
              new-tile (util/xy-or-nil carte new-coord)]
          (case new-tile
            nil false ;; we're out
            \# (recur coord (util/rotate-90-clockwise dir) visited)
            (recur new-coord dir (conj visited [coord dir])))))))

(let [input (-> (util/slurp-input) parse-input)]
  ;; we only try coords that were in the path,
  ;; altough this is only about x3 speedup
  ;; so we could have abstained.
  (->> (compute-visited-coords input)
       (remove #(= (:start-coord input) %))
       (map (fn [[x y]]
              (assoc-in input [:carte y x] \#)))
       (filter will-loop?)
       count))
