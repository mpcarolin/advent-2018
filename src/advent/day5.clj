(ns advent.day5
  (:require [advent.core :as core]
            [clojure.string :as s]))

(def polymers (first (core/get-input 5)))

;; ======== PART 1 ======== 

(defn same-type?
  [& letters]
  (let [lower-pair (map s/lower-case letters)]
    (apply = lower-pair)))

(defn same-polarity?
  [& letters]
  (or (every? #(Character/isUpperCase %) letters)
      (every? #(Character/isLowerCase %) letters)))

(def opposite-polarity? (complement same-polarity?))

(defn reacts?
  "Returns true if the pair of polymer units react"
  [a b]
  (and (not-any? nil? [a b])
       (apply same-type? [a b])
       (apply opposite-polarity? [a b])))

(defn react-reducer
  [finished unit]
  (if-let [prev (peek finished)]
    (if (reacts? unit prev)
      (pop finished)
      (conj finished unit))
    [unit]))

(defn react [polymer] (reduce react-reducer [] polymer))

(def part1-result
  (let [reacted (react polymers)]
    {:reacted reacted
     :count (count reacted)}))

;; ======== PART 2 ======== 

(defn chars-of
  [unit]
  (conj #{}
        (Character/toLowerCase unit)
        (Character/toUpperCase unit)))

(defn remove-unit
  "Returns s with unit removed"
  [s unit]
  (->> s
      (remove (chars-of unit))
      (apply str)))

(defn find-optimal-reaction
  "Removes the unit type from s that is blocking the most reactions and then reacts the result."
  [polymers]
  (let [units (set (s/lower-case polymers))
        permutations (map #(remove-unit polymers %) units)
        reactions (map react permutations)]
    (apply min-key count reactions)))

(def part2-result
  (let [result (find-optimal-reaction polymers)]
    {:result result
     :count (count result)}))

