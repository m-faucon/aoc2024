(ns aoc2024.day9
  (:require
    [aoc2024.util :as util]))

(defn parse-input
  [s]
  (->> s
       (map (comp parse-long str))
       (partition-all 2)
       (map-indexed (fn [id [size empty-size-after]]
                      (concat (repeat size id) (repeat (or empty-size-after 0) nil))))
       (into [] cat)))

(defn find-nil-idx
  [v start-idx]
  (->> (range)
       (map (partial + start-idx))
       (filter (comp nil? v))
       first))

(defn find-non-nil-idx-from-end
  [v start-idx]
  (->> (range)
       (map (partial - start-idx))
       (filter v)
       first))

(defn compact
  [v]
  (loop [v v
         left 0
         right (dec (count v))]
    (let [to-move (find-non-nil-idx-from-end v right)
          where (find-nil-idx v left)]
      (if (< where to-move)
        (recur (-> (assoc v to-move nil)
                   (assoc where (v to-move)))
               (inc where)
               (dec to-move))
        v))))

(defn checksum
  [v]
  (->> v
       (take-while some?)
       (map-indexed *)
       (reduce +)))

(-> (util/slurp-input)
    parse-input
    compact
    checksum)

;; part2

;; WARNING: This is both ugly an inefficient
;; It is n^2. It operates on the "disk" level,
;; not on a more compact representation
;; (fortunately files and gaps are never more than 9 blocks)
;;
;; Excuse : I am pretty feverish atm


;; ugh...
(defn positions
  [disk]
  (->> (map-indexed (fn [idx id] [id idx]) disk)
       (partition-by first)
       (remove (comp nil? first first))
       (map (fn [[[id idx] :as xs]] {:id id :idx idx :cnt (count xs)}))
       reverse))

(defn parse-input2
  [s]
  (let [disk (parse-input s)]
    {:disk disk :positions (positions disk)}))

;; ugh2...
(defn find-free-space
  [v n]
  (->> (range (- (count v) n -1))
       (filter (fn [idx]
                 (every? nil? (subvec v idx (+ idx n)))))
       first))

;; ugh3...
(defn replace-n
  [v start-idx n x]
  (reduce (fn [v idx]
            (assoc v idx x))
          v
          (range start-idx (+ start-idx n))))

;; ugh4...
(defn compact2
  [{:keys [disk positions]}]
  (reduce (fn [disk {:keys [id idx cnt]}]
            (let [free-idx (find-free-space disk cnt)]
              (if (and free-idx (< free-idx idx))
                (-> disk
                    (replace-n free-idx cnt id)
                    (replace-n idx cnt nil))
                disk)))
          disk
          positions))

(defn checksum2
  [v]
  (->> v
       (keep-indexed (fn [idx id] (when id (* idx id))))
       (reduce +)))

(-> (util/slurp-input)
    parse-input2
    compact2
    checksum2)
