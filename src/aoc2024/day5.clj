(ns aoc2024.day5
  (:require
   [aoc2024.util :as util]
   [clojure.set :as set]
   [clojure.string :as str]))

(defn parse-rule
  [s]
  (mapv parse-long (str/split s #"\|")))

(defn parse-page
  [s]
  (mapv parse-long (str/split s #",")))

(defn parse-input
  [s]
  (let [[r p] (str/split s #"\n\n")]
    [(->> (str/split-lines r)
          (map parse-rule)
          (reduce (fn [acc [k v]]
                    (update acc k (fnil conj #{}) v))
                  {}))
     (map parse-page (str/split-lines p))]))

(defn i-n-subseqs
  [xs]
  (->> (iterate rest xs)
       (take-while seq)))

(defn ok-first-elt?
  [rules [id & ids]]
  (every? (rules id) ids))

(defn ok-page?
  [rules ids]
  (every? (partial ok-first-elt? rules) (i-n-subseqs ids)))

(defn middle-elt
  [v]
  (v (/ (dec (count v)) 2)))

(let [[rules pages] (parse-input (util/slurp-input))]
  (->> (filter (partial ok-page? rules) pages)
       (map middle-elt)
       (reduce +)))

;; part2

;; shall we implement topological sort?
;; or... let's inspect the data and see
;; if we can cheat a bit

(let [[rules pages] (parse-input (util/slurp-input))
      bad-page (first (remove (partial ok-page? rules) pages))
      restricted-graph (-> (select-keys rules bad-page)
                           (update-vals #(set/intersection % (set bad-page))))]
  restricted-graph)

;; {91 #{21 76 45 78 81},
;;  76 #{21 45 78 81},
;;  94 #{21 91 76 45 78 81},
;;  81 #{21 45 78},
;;  45 #{21 78},
;;  78 #{},
;;  21 #{78}}
;; oh. It looks like for this one, the number of elements that
;; are before a given one is always a different number :thinking-face:
;; That must mean there is only one correct sort for this one, and that
;; it is very easy to compute : just count the number of edges, and sort
;; by the opposite of that.

;; Let's check that this applies for all input pages :

(let [[rules pages] (parse-input (util/slurp-input))]
  (->> (remove (partial ok-page? rules) pages)
       (map (fn [page]
              (->> (select-keys rules page)
                   vals
                   (map #(clojure.set/intersection % (set page)))
                   (map count)
                   frequencies
                   vals
                   set)))
       set))

;; => #{#{1}}
;; It does. We don't have to to fancy topological sort then.

(defn restricted-graph
  [rules page]
  (-> (select-keys rules page)
      (update-vals #(set/intersection % (set page)))))

(defn sort-page
  [rules page]
  (sort-by (comp - count (restricted-graph rules page))
           page))

(let [[rules pages] (parse-input (util/slurp-input))]
  (->> (remove (partial ok-page? rules) pages)
       (map (partial sort-page rules))
       (map (comp middle-elt vec))
       (reduce +)))
