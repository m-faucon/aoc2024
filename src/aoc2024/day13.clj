(ns aoc2024.day13
  (:require [clojure.string :as str]
            [aoc2024.util :as util]))

(def section-regex #"Button A: X\+(\d+), Y\+(\d+)\nButton B: X\+(\d+), Y\+(\d+)\nPrize: X=(\d+), Y=(\d+)")

(defn parse-section
  [s]
  (let [[ax ay bx by px py]
        (->> (re-find section-regex s)
             rest
             (map parse-long))]
    {:ax ax :ay ay :bx bx :by by :px px :py py}))

(defn parse-input
  [s]
  (->> (str/split s #"\n\n")
       (map parse-section)))

(def a-cost 3)

(def b-cost 1)

(defn token-cost
  [[a b]]
  (+ (* a a-cost) (* b b-cost)))

(def max-brute-force-presses 100)

(defn brute-force
  [{:keys [ax ay bx by px py]}]
  (->> (for [a (range max-brute-force-presses)
             b (range max-brute-force-presses)]
         [[a b]
          (+ (* a ax) (* b bx))
          (+ (* a ay) (* b by))])
       (filter #(= [px py] (rest %)))
       ;; turns out there is 0 or 1 solutions for each
       ;; so this is was just deception from aoc but...
       (map (comp token-cost first))
       (sort)
       first))

(->> (parse-input (util/slurp-input))
     (keep brute-force)
     (reduce +))

;; part2

;; NOTE(edit):
;; wew, looking at other's solutions, it seems I much overcomplicated it.
;; I try to solve the diophantine equations indepentantly, then to find
;; a common solution i solve a 2x2 system
;; But... I could just solve the input as a 2x2 system to begin with...
;; whatever

(defn update-section-part2
  [section]
  (-> section
      (update :px + 10000000000000)
      (update :py + 10000000000000)))

(defn extended-euclid
  [a b]
  (loop [r a u 1 v 0 r' b u' 0 v' 1]
    (if (not (zero? r'))
      (let [q (long (/ r r'))]
        (recur r' u' v' (- r (* q r')) (- u (* q u')) (- v (* q v'))))
      [r u v])))

(defn solutions-line
  [a b c]
  (let [[d u v] (extended-euclid a b)
        a' (/ a d)
        b' (/ b d)
        c' (/ c d)
        x0 (*' u c')
        y0 (*' v c')]
    (when (integer? c')
      [x0 y0 a' b'])))

(defn solution-for-k
  [[x0 y0 a' b'] k]
  [(+ x0 (* k b')) (- y0 (* k a'))])

(defn solve-2x2
  [a b c a' b' c']
  (when-not (zero? (-' (*' a b') (*' a' b)))
    [(/ (-' (*' c b') (*' c' b))
        (-' (*' a b') (*' a' b)))
     (/ (-' (*' a c') (*' a' c))
        (-' (*' a b') (*' a' b)))]))


;; X_x0 + k*X_b' = Y_x0 + k'*Y_b'
;; => X_b'*k - Y_b'*k' = Y_x0 - X_x0
;; and
;; X_y0 - k*X_a' = Y_y0 - k'*Y_a'
;; => X_a'*k - Y_a'*k' = X_y0 - Y_y0

;; NOTE: Notation is very messy mb
;; capital `X` and `Y` refer to the original
;; problem x and y, while lower case `x` and `y`
;; refer to solutions of the diophantine equations,
;; so the number of presses
;; `a'` and `b'` refer to the normalized `a` and `b` coeffs
;; of the diophantine equations

(defn solve-machine
  [{:keys [ax bx ay by px py]}]
  (let [[X_x0 X_y0 X_a' X_b' :as sline-X] (solutions-line ax bx px)
        [Y_x0 Y_y0 Y_a' Y_b' :as sline-Y] (solutions-line ay by py)]
    (when (and sline-X sline-Y)
      (let [[X_k Y_k]
            (solve-2x2 X_b' (- Y_b') (- Y_x0 X_x0)
                       X_a' (- Y_a') (- X_y0 Y_y0))]
        ;; NOTE: if the det is 0, then there is 0 or infinite solutions
        ;; It could be that there are infinite solutions. In this case
        ;; it means we should ignore one of the equations, and then search
        ;; the solutions for the one with minimal cost.
        ;; However it seems it doesn't happen in the input.
        (when (and (integer? X_k) (integer? Y_k))
          (token-cost (solution-for-k sline-X X_k)))))))

(->> (parse-input (util/slurp-input))
     (map update-section-part2)
     (keep solve-machine)
     (reduce +))
