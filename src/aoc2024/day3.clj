(ns aoc2024.day3
  (:require
    [aoc2024.util :as util]
    [clojure.string :as str]))

(def mul-regex #"mul\((\d{1,3}),(\d{1,3})\)")

(->> (re-seq mul-regex (util/slurp-input))
     (map (fn [[_ l r]]
            (* (parse-long l) (parse-long r))))
     (reduce +))

;; part 2

(def do-regex #"do\(\)")
(def dont-regex #"don't\(\)")

(defn concat-patterns
  [& args]
  (re-pattern (str/join "|" args)))

(def instruction-regex (concat-patterns mul-regex do-regex dont-regex))

(defn do-dont-xf
  "Stateful transducer that discards everything after a `dont-token` until the next `do-token`.
  Also discards `do-token` and `dont-token`. Meant for single-threaded use."
  [do-token dont-token]
  (let [pass (atom true)]
    (fn [rf]
      (completing
       (fn [acc x]
         (if @pass
           (do
             (when (= dont-token x)
               (reset! pass false))
             (if (or (= do-token x) (= dont-token x))
               acc
               (rf acc x)))
           (do
             (when (= do-token x)
               (reset! pass true))
             acc)))))))

(comment (sequence (do-dont-xf :do :dont) [1 :dont 2 3 :do 4 5]) :=> '(1 4 5))

(->> (re-seq instruction-regex (util/slurp-input))
     (transduce
      (comp (do-dont-xf ["do()" nil nil] ["don't()" nil nil])
            (map (fn [[_ l r]]
                   (* (parse-long l) (parse-long r)))) )
      +))
