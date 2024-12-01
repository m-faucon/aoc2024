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
