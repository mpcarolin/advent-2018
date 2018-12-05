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
