(ns advent.day5
  (:require [advent.core :as core]
            [clojure.string :as s]))

(def polymers (core/get-input 5))

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
  [pair]
  (and (apply same-type? pair)
       (apply opposite-polarity? pair)))

(defn char-vec [s] (map identity s))
(defn vec->str [v] (apply str v))

(defn react-polymer-once
  [polymers]
  (loop [[a b & rest] (char-vec polymers)
         result []]
    (cond
      (nil? a) (vec->str result)
      (nil? b) (vec->str (conj result a))
      (reacts? [a b]) (recur rest result)
      :else (recur (cons b rest)
                   (conj result a)))))

(defn react-polymers
  [polymers]
  (loop [polys polymers]
    (let [result (react-polymer-once polys)]
      (if (= result polys)
        polys
        (recur result)))))

;; ======== PART 2 ======== 
