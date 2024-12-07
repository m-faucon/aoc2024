(ns aoc2024.day7
  (:require
    [clojure.string :as str]
    [aoc2024.util :as util]))

;; solution explanation :
;; in order to not waste time pursuing useless branches,
;; we start with the last number. If it doesnt divide the target,
;; then the last op can't be multiply. Also if it's larger than the target,
;; the last op can't be add.
;; So we build up the tree this way, adding 0, 1 or 2 children to the queue.
;; The children have a different target and one less number.
;; It's cutting off most branches very early, so the solution runs instantly.
;; We use `tree-seq` to avoid maintaining the queue ourselves.

(defn parse-line
  [s]
  (let [[target numbers] (str/split s #": ")]
    {:target (parse-long target)
     :numbers (->> (str/split numbers #" ")
                   (map parse-long)
                   ;; as stated we start with the last number
                   ;; and work backwards
                   reverse)}))

(defn one-less-number
  [{:keys [target numbers]}]
  (let [[f & r] numbers
        divided-target (/ target f)
        subtracted-target (- target f)]
    (cond-> []
      (integer? divided-target)
      (conj {:target divided-target
             :numbers r})
      (pos? subtracted-target)
      (conj {:target subtracted-target
             :numbers r}))))

(defn branch?
  [{:keys [numbers]}]
  (<= 2 (count numbers)))

(defn good-leaf?
  [{:keys [target numbers]}]
  (= target (first numbers)))

(defn good-line?
  [parsed-line]
  (->> (tree-seq branch? one-less-number parsed-line)
       (some good-leaf?)))

(->> (util/input-lines)
     (map parse-line)
     (filter good-line?)
     (map :target)
     (reduce +))

;; part2

;; just need to add another op, or rather the reverse op.

(defn uncat-numbers
  "`z` such that `z || y = x` where `||` is digit concatenation.
  Or `nil` if no such `z`."
  [x y]
  (let [x (str x)
        y (str y)
        nx (count x)
        ny (count y)]
    (when (and (< ny nx)
               (= y (subs x (- nx ny))))
      (parse-long (subs x 0 (- nx ny))))))

(defn one-less-number2
  [{:keys [target numbers]}]
  (let [[f & r] numbers
        divided-target (/ target f)
        subtracted-target (- target f)
        uncatted-target (uncat-numbers target f)]
    (cond-> []
      (integer? divided-target)
      (conj {:target divided-target
             :numbers r})
      (pos? subtracted-target)
      (conj {:target subtracted-target
             :numbers r})
      uncatted-target
      (conj {:target uncatted-target
             :numbers r}))))

(defn good-line2?
  [parsed-line]
  (->> (tree-seq branch? one-less-number2 parsed-line)
       (some good-leaf?)))

(->> (util/input-lines)
     (map parse-line)
     (filter good-line2?)
     (map :target)
     (reduce +))
