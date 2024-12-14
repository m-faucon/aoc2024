(ns aoc2024.util
  (:require
   [clojure.string :as str]))

(defn day
  []
  (->> (str/split (str *ns*) #"\.")
       last
       (re-find #"\d+")
       parse-long))

(defn slurp-input
  ([] (slurp-input 1))
  ; the trim is because when I copy paste into emacs and save it adds a newline at the end
  ([i] (str/trimr (slurp (str "resources/day" (day) "-" i)))))

(defn input-lines
  ([] (input-lines 1))
  ([i]
   (str/split-lines (slurp-input i))))

(def up [0 -1])

(def down [0 1])

(def left [-1 0])

(def right [1 0])

(defn xy-or-nil
  [carte [x y]]
  (try ((carte y) x) (catch IndexOutOfBoundsException _ nil)))

(def rotate-90-clockwise
  {up right
   right down
   down left
   left up})

(defn v+
  [[x0 y0] [x1 y1]]
  [(+ x0 x1) (+ y0 y1)])

(defn v-
  [[x0 y0] [x1 y1]]
  [(- x0 x1) (- y0 y1)])

(defn v*
  [a [x y]]
  [(* a x) (* a y)])

(defn inside?
  [W H [x y]]
  (and (<= 0 x (dec W)) (<= 0 y (dec H))))

(defn man-neighbours
  [[x y]]
  [[(inc x) y]
   [x (inc y)]
   [(dec x) y]
   [x (dec y)]])
