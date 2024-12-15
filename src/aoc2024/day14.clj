(ns aoc2024.day14
  (:require [aoc2024.util :as util]))


(def W 101)

(def H 103)

(def number-regex #"[-\d]+")
(def line-regex (re-pattern
                 (format "p=(%s),(%s) v=(%s),(%s)"
                         number-regex number-regex number-regex number-regex)))
(defn parse-line
  [s]
  (let [[px py vx vy] (->> (re-find line-regex s)
                           rest
                           (map parse-long))]
    {:p [px py]
     :v [vx vy]}))

(defn robot-position-after-n
  [n robot]
  (let [{:keys [p v]} robot
        [init-px init-py] p
        [vx vy] v]
    [(-> init-px
         (+ (* n vx))
         (mod W))
     (-> init-py
         (+ (* n vy))
         (mod H))]))

(defn quadrant
  [[x y]]
  (let [half-point-horizontal (/ (dec W) 2)
        horizontal (cond
                     (< x half-point-horizontal) :left
                     (> x half-point-horizontal) :right)
        half-point-vertical (/ (dec H) 2)
        vertical (cond
                   (< y half-point-vertical) :top
                   (> y half-point-vertical) :bottom)]
    (when (and horizontal vertical)
      [horizontal vertical])))

(->> (util/input-lines)
     (map parse-line)
     (map (partial robot-position-after-n 100))
     (keep quadrant)
     frequencies
     vals
     (reduce *))

;; part2

;; wtf is a "picture of a christmas tree"?
;; well I assume, that a constraint would be that
;; robots don't overlap. So let's find that first

(let [input (->> (util/input-lines)
                 (map parse-line))
      [idx robots]
      (->> (for [idx (range)]
             [idx (map (partial robot-position-after-n idx) input)])
           (filter #(every? #{1} (-> % second frequencies vals)))
           first)]
  (def idx idx)
  (def robots robots))

;; now let's display it and check if it looks
;; like a christmas tree indeed.

(defn display
  [robots]
  (let [robots (set robots)]
    (doseq [y (range H)]
      (->> (for [x (range W)]
             (if (robots [x y]) "X" "."))
           (apply str)
           (println)))))

(display robots)

;; alright, seems like it.
;; so the answer is

idx
