(ns aoc2024.day8
  (:require
   [aoc2024.util :as util]))

(defn parse-input
  [lines]
  {:antennas (->> lines
                  (map-indexed
                   (fn [y line]
                     (keep-indexed
                      (fn [x c]
                        (when (not= \. c)
                          [[x y] c]))
                      line)))
                  (sequence cat)
                  (reduce (fn [acc [coord c]]
                            (update acc c (fnil conj []) coord))
                          {}))
   :W (count (first lines))
   :H (count lines)})

(defn pairs
  [v]
  (for [i (range (count v))
        j (range i)]
    [(v i) (v j)]))

(defn anti-nodes
  [[ant1 ant2]]
  (let [v (util/v- ant2 ant1)]
    [(util/v+ ant1 (util/v* -1 v))
     (util/v+ ant2 v)]))

(defn inside?
  [W H [x y]]
  (and (<= 0 x (dec W)) (<= 0 y (dec H))))

(let [{:keys [antennas W H]} (-> (util/input-lines) (parse-input))]
  (->> (vals antennas)
       (mapcat pairs)
       (mapcat anti-nodes)
       (filter (partial inside? W H))
       set
       count))

;; part2

(defn anti-nodes2
  [W H [ant1 ant2]]
  ;; NOTE: if `v` is an integer multiple of an integer vector,
  ;; then we should use that smallest integer vector as `v`
  ;; instead, to follow most closely the words to the problem.
  ;; ("any that's aligned regardless of distance")
  ;; However, either the situation doesn't arise, or the words were
  ;; unclear, because this works as is.
  (let [v (util/v- ant2 ant1)]
    (concat
     (->> (iterate (partial util/v+ v) ant2)
          (take-while (partial inside? W H)))
     (->> (iterate (partial util/v+ (util/v* -1 v)) ant1)
          (take-while (partial inside? W H))))))

(let [{:keys [antennas W H]} (-> (util/input-lines) (parse-input))]
  (->> (vals antennas)
       (mapcat pairs)
       (mapcat (partial anti-nodes2 W H))
       set
       count))
