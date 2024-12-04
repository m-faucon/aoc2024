(ns aoc2024.day4
  (:require
   [aoc2024.util :as util]))

(defn transpose
  [lines]
  (apply mapv vector lines))

(defn xy-get
  [lines x y]
  ((lines y) x))

(defn diagonals-nw-se
  [lines]
  (let [lines (mapv vec lines)
        L (count (first lines))
        H (count lines)]
    (concat
     (for [x (range L)]
       (vec (for [i (range (min H (- L x)))]
              (xy-get lines (+ x i) i))))
     (for [y (range 1 H)]
       (vec (for [i (range (min L (- H y)))]
              (xy-get lines i (+ y i))))))))

(defn all-lines
  [lines]
  (concat
   lines
   (transpose lines)
   (diagonals-nw-se lines)
   (diagonals-nw-se (map reverse lines))))

(->> (util/input-lines)
     (all-lines)
     (map #(apply str %))
     (map (fn [line]
            (+ (count (re-seq #"XMAS" line))
               (count (re-seq #"SAMX" line)))))
     (reduce +))

;; part2

;; ugh...
(defn sub-3x3-matrix-at
  [lines x y]
  [[(xy-get lines (+ 0 x) (+ 0 y))
    (xy-get lines (+ 1 x) (+ 0 y))
    (xy-get lines (+ 2 x) (+ 0 y))]
   [(xy-get lines (+ 0 x) (+ 1 y))
    (xy-get lines (+ 1 x) (+ 1 y))
    (xy-get lines (+ 2 x) (+ 1 y))]
   [(xy-get lines (+ 0 x) (+ 2 y))
    (xy-get lines (+ 1 x) (+ 2 y))
    (xy-get lines (+ 2 x) (+ 2 y))]])

(defn all-3x3-submatrices
  [lines]
  (let [lines (mapv vec lines)
        L (count (first lines))
        H (count lines)]
    (for [x (range (- L 2))
          y (range (- H 2))]
      (sub-3x3-matrix-at lines x y))))

(defn xmas-submatrix?
  [m]
  (let [m-normalized
        (-> m
            (assoc-in [0 1] nil)
            (assoc-in [1 0] nil)
            (assoc-in [2 1] nil)
            (assoc-in [1 2] nil))]
    (case m-normalized
      ([[\M nil \S]
        [nil \A nil]
        [\M nil \S]]
       [[\M nil \M]
        [nil \A nil]
        [\S nil \S]]
       [[\S nil \S]
        [nil \A nil]
        [\M nil \M]]
       [[\S nil \M]
        [nil \A nil]
        [\S nil \M]])
      true
      false)))

(->> (util/input-lines)
     (all-3x3-submatrices)
     (filter xmas-submatrix?)
     count)
