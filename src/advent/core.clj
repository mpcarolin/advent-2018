(ns advent.core
  (:require [clojure.spec.alpha :as sp]
            [clojure.spec.test.alpha :as stest]
            [clojure.spec.gen.alpha :as gen]))

(defn parse-int [x] (Integer/parseInt x))

(defn get-input
  ([day]
  (let [path (str "src/advent/input/day" day ".txt")
        content (slurp path)]
    (clojure.string/split-lines content)))
  ([day alter-fn]
    (let [result (get-input day)]
      (map alter-fn result))))

;; Day 1 problem 1
(defn sum-frequencies
  [deltas]
  (reduce + deltas))

(defn first-repeated-freq
  [deltas]
  (loop [past-set #{0}
         sum 0
         [delta & rest] deltas]
    (let [new-sum (+ sum delta)]
      (if (contains? past-set new-sum)
        new-sum
        (recur (conj past-set new-sum)
               new-sum
               (or rest deltas))))))

;; day 2
(defn update-else
  "Updates k in m using f, if k exists. Else assocs
   k in m with value default"
  [m k f default]
  (if (contains? m k)
    (update m k f)
    (assoc m k default)))

(defn char-freq
  "Returns a tuple of two integers. The first value, 0 or 1, indicates
   string has a character seen exactly twice. The second value, exactly
   three times."
  [s]
  (let [count-reducer (fn [counts char]
                        (update-else counts char inc 1))
        counts (->> s
                    (reduce count-reducer {})
                    (vals)
                    (set))]
    (vector (if (contains? counts 2) 1 0)
            (if (contains? counts 3) 1 0))))

(defn checksum
  [ids]
  (let [counts (map char-freq ids)
        sum-fn (fn [[sum2 sum3] [count2 count3]]
                (vector (+ sum2 count2)
                        (+ sum3 count3)))
        sums (reduce sum-fn counts)]
    (reduce * sums)))

;; day2 part 2
(defn similar-ids?
  [a b]
  (let [label (fn [x y] (if (= x y) :match :mismatch))]
    (->> (map label a b)
         (filter #{:mismatch})
         (count)
         (>= 1))))

(defn get-shared-chars
  [a b]
  (letfn [(match-or-nil
           [x y]
           (if (= x y) x nil))]
    (->> (map match-or-nil a b)
         (remove nil?)
         (apply str))))

(defn similar-pair
  [ids]
  (first
    (for [i (range 0
                   (count ids))
          j (range (inc i)
                   (count ids))
          :let [id (ids i)
                counterpart (ids j)]
          :when (similar-ids? id
                              counterpart)]
       (get-shared-chars id
                         counterpart))))



;; day 3
;;; Input parsing

(defn seq-split [s re] (seq (.split s re)))
(defn parse-dist
  [s]
  (let [[l x] (seq-split s ",")
        end (dec (count x))
        t (.substring x 0 end)]
    [(parse-int l)
     (parse-int t)]))

(defrecord Claim [id l t w h])

(defn line->claim
  [line]
  (let [split (seq-split line " ")
        id (.substring (first split) 1)
        [left-dist top-dist] (parse-dist (nth split 2))
        [w h] (map parse-int
                (seq-split (last split) "x"))]
    (->Claim id left-dist top-dist w h)))


(comment
(def claim (first claims))
(def t (:t claim))
(def l (:l claim))
(def w (:w claim))
(def h (:h claim))
)

(defn fill-cells
  "Cells: map of coordinates (tuple vector) to existence counts"
  [cells {:keys [l t w h] :as claim}]
  (let [coords (for [row (range t (+ t h))
                     col (range l (+ l w))]
                 [row col])]
    (reduce (fn [cells coord]
              (update-else cells coord inc 1))
            cells
            coords)))

(defn countp
  "Counts the number of elements in col which satisfy pred."
  [pred col]
  (count
   (filter pred col)))

(defn count-overlaps
  [cells]
  (countp #(>= % 2)
          (vals cells)))

;; raw input
(defn compute-overlap-answer
  [lines]
  (let [claims (map line->claim lines)
        cells (reduce fill-cells {} claims)]
    (count-overlaps cells)))

(def lines (get-input 3))

(compute-overlap-answer lines)
