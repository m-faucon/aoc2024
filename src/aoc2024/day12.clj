(ns aoc2024.day12
  (:require
   [clojure.string :as str]
   [aoc2024.util :as util]))

(defn parse-input
  [s]
  (let [lines (mapv vec (str/split-lines s))
        W (count (first lines))
        H (count lines)]
    {:xy->label (into {}
                      (for [x (range W) y (range H)]
                        [[x y] ((lines y) x)]))
     :W W
     :H H}))

(defn merge-ccs
  [{:keys [xy->cc-id cc-id->xys]} xy1 xy2]
  (let [cc2-id (xy->cc-id xy2)
        cc1-id (xy->cc-id xy1)
        cc2-xys (cc-id->xys cc2-id)]
    {:xy->cc-id
     (reduce (fn [acc xy] (assoc acc xy cc1-id))
             xy->cc-id cc2-xys)
     :cc-id->xys
     (-> (dissoc cc-id->xys cc2-id)
         (update cc1-id into cc2-xys))}))

(defn add-to-cc
  [ccs new-xy cc-xy]
  (let [cc-id (get-in ccs [:xy->cc-id cc-xy])]
    (-> ccs
        (update :xy->cc-id assoc new-xy cc-id)
        (update-in [:cc-id->xys cc-id] conj new-xy))))

(defn same-cc?
  [{:keys [xy->cc-id]} xy1 xy2]
  (= (xy->cc-id xy1) (xy->cc-id xy2)))

(defn add-new-cc
  [ccs xy id]
  (-> ccs
      (update :xy->cc-id assoc xy id)
      (update :cc-id->xys assoc id [xy])))

(defn left-coord
  [[x y]]
  [(dec x) y])

(defn up-coord
  [[x y]]
  [x (dec y)])

(defn compute-ccs
  [{:keys [xy->label W H]}]
  (reduce (fn [ccs xy]
            (let [label (xy->label xy)
                  left-coord (left-coord xy)
                  left-label (xy->label left-coord)
                  up-coord (up-coord xy)
                  up-label (xy->label up-coord)]
              (cond
                (= label left-label up-label)
                (cond-> (add-to-cc ccs xy left-coord)
                  (not (same-cc? ccs left-coord up-coord))
                  (merge-ccs left-coord up-coord))
                (= label left-label)
                (add-to-cc ccs xy left-coord)
                (= label up-label)
                (add-to-cc ccs xy up-coord)
                :else
                (let [new-id (random-uuid)]
                  (add-new-cc ccs xy new-id)))))
          {:xy->cc-id {} :cc-id->xys {}}
          ;; order matters since we look at left and up neighbours
          (for [y (range H) x (range W)] [x y])))

(defn sides-fenced-count
  [xy->label xy]
  (let [label (xy->label xy)]
    (->> (util/man-neighbours xy)
         (map xy->label)
         (filter #(not= label %))
         count)))

(defn cc-fencing-cost
  [xy->label xys]
  (->> (map (partial sides-fenced-count xy->label) xys)
       (reduce +)
       (* (count xys))))

(let [{:keys [xy->label] :as input} (parse-input (util/slurp-input))
      ccs (compute-ccs input)]
  (->> (:cc-id->xys ccs) vals
       (map (partial cc-fencing-cost xy->label))
       (reduce +)))

;; part2

(defn man-neighbours-labeled
  [xy]
  (map vector
       (util/man-neighbours xy)
       [:right :down :left :up]))

(defn sides-fenced
  [xy->label xy]
  (let [label (xy->label xy)]
    (->> (man-neighbours-labeled xy)
         (keep (fn [[neigh-xy neigh-kind]]
                 (when (not= label (xy->label neigh-xy))
                   [xy neigh-kind]))))))

(defn group-fences
  [fences]
  (reduce (fn [acc [xy fence-kind]]
            (let [[grouping-dim other-dim]
                  (case fence-kind
                    (:down :up) [1 0]
                    (:right :left) [0 1])]
              (update acc
                      [fence-kind (xy grouping-dim)]
                      (fnil conj [])
                      (xy other-dim))))
          {}
          fences))

(defn ccs-1d
  [xs]
  (let [sorted (sort xs)]
    (loop [current-cc []
           ccs []
           prev (dec (first sorted))
           remaining sorted]
      (if-let [[f & r] (seq remaining)]
        (if (= f (inc prev))
          (recur (conj current-cc f) ccs f r)
          (recur [f] (conj ccs current-cc) f r))
        (conj ccs current-cc)))))

(defn cc-fencing-cost2
  [xy->label xys]
  (->> (mapcat (partial sides-fenced xy->label) xys)
       group-fences
       vals
       (mapcat ccs-1d)
       count
       (* (count xys))))

(let [{:keys [xy->label] :as input} (parse-input (util/slurp-input))
      ccs (compute-ccs input)]
  (->> (:cc-id->xys ccs) vals
       (map (partial cc-fencing-cost2 xy->label))
       (reduce +)))
