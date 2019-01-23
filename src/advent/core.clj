(ns advent.core
  (:require [clojure.spec.alpha :as sp]
            [clojure.spec.test.alpha :as stest]
            [clojure.spec.gen.alpha :as gen]))

(defn parse-int [x] (Integer/parseInt x))

(defn seq-split
  "Splits the string s by the regex re, then returns the result as a seq."
  [s re]
  (seq (.split s re)))

(defn get-input
  ([day]
  (let [path (str "src/advent/input/day" day ".txt")
        content (slurp path)]
    (clojure.string/split-lines content)))
  ([day alter-fn]
    (let [result (get-input day)]
      (map alter-fn result))))

;; ==========================================================================;;

;; Day 1 part 1
(defn sum-frequencies
  [deltas]
  (reduce + deltas))

;; part 2
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

;; ==========================================================================;;
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

;; ==========================================================================;;
;; Day 3

(defn parse-dist
  [s]
  (let [[l x] (seq-split s ",")
        end (dec (count x))
        t (.substring x 0 end)]
    [(parse-int l)
     (parse-int t)]))

(defrecord Dimensions [l t w h])
(defrecord Claim [id dimensions coordinates])

(defn coordinates
  [l t w h]
  (for [row (range t (+ t h))
        col (range l (+ l w))]
    [row col]))

(defn line->claim
  [line]
  (let [split (seq-split line " ")
        id (.substring (first split) 1)
        [left-dist top-dist] (parse-dist (nth split 2))
        [w h] (map parse-int
                (seq-split (last split) "x"))
        coords (coordinates left-dist top-dist w h)]
    (->Claim id
             (->Dimensions left-dist top-dist w h)
             coords)))

(defn fill-cells
  "Cells: map of coordinates (tuple vector) to existence counts"
  [cells {:keys [id dimensions coordinates] :as claim}]
  (reduce (fn [cells coord]
            (update-else cells coord inc 1))
          cells
          coordinates))

(defn countp
  "Counts the number of elements in col which satisfy pred."
  [pred col]
  (count
   (filter pred col)))

(defn count-overlaps
  [cells]
  (countp #(>= % 2)
          (vals cells)))

(defn compute-overlap-answer
  [lines]
  (let [claims (map line->claim lines)
        cells (reduce fill-cells {} claims)]
    (count-overlaps cells)))

;; Day 3 Part 2: locate isolated claim
(defn isolated?
  [{:keys [coordinates]} cell-counts]
  (letfn [(no-overlap? [coordinate]
           (= 1 (get cell-counts coordinate)))]
    (every? no-overlap? coordinates)))

(defn find-isolated-claim
  [lines]
  (let [claims (map line->claim lines)
        cells (reduce fill-cells {} claims)]
    (->> claims
         (filter #(isolated? % cells))
         (first)
         (:id))))

;; ==========================================================================;;
;; Day 4
;; input
(def lines (get-input 4))

(defn not-zero? [x] (not= 0 x))

(defrecord datetime [year month day minute]
  java.lang.Comparable
  (compareTo [{:keys [year month day minute]} {year2 :year month2 :month day2 :day minute2 :minute}]
    (let [year-diff (compare year year2)
          month-diff (compare month month2)
          day-diff (compare day day2)
          minute-diff (compare minute minute2)]
        (cond
          (not-zero? year-diff) year-diff
          (not-zero? month-diff) month-diff
          (not-zero? day-diff) day-diff
          :else minute-diff))))

(defrecord GuardEvent [date text]
  java.lang.Comparable
  (compareTo [{date :date} {other-date :date}]
             (compare date other-date)))

(defn str->datetime
  [s]
  (let [[date-str minute-str] (seq-split s " ")
        [year month day] (seq-split date-str "-")
        [_ minute] (seq-split minute-str ":")
        args (map parse-int [year month day minute])]
    (apply ->datetime args)))

(defn Line->GuardEvent
  [line]
  (let [[date-str text] (seq-split line "]")
        date (-> date-str
                 (.substring 1)
                 (str->datetime))]
    (->GuardEvent date text)))

(defn guard-id
  [s]
  (if-let [hashtag-index (clojure.string/index-of s "#")]
    (let [remainder (.substring s (inc hashtag-index))
          [id & rem] (clojure.string/split remainder #" ")]
      (parse-int id))
    nil))

(defn guard-map
  "Takes a GuardEvent, and returns a map for that event including :id"
  [{:keys [date text] :as guard-event}]
  (if-let [id (guard-id text)]
    {:id id :event guard-event}
    {:id nil :event guard-event}))

(defn list-assoc
  "Conj's v to the list mapped by k in m, if k exists in m. Otherwise,
   creates a new list mapped by k and conj's v to it."
  [m k v]
  (if-let [lst (get m k)]
    (assoc m k (conj lst v))
    (assoc m k v)))

(defn reduce-guards
  "Takes all guard events and groups them together into a map, keyed by guard id to lists of events"
  [guard-events]
  (let [guards (map guard-map guard-events)
        grouped (partition-by (comp nil? :id) guards)
        partitioned (partition 2 grouped)
        merged-by-id (map #(apply concat %) partitioned)]
    (reduce (fn [guards-by-id events-for-single-guard]
              (let [id (:id (first events-for-single-guard))]
                (list-assoc guards-by-id id events-for-single-guard)))
            {}
            merged-by-id)))

(defn asleep?
  [event-text]
  (clojure.string/includes? event-text "falls asleep"))

(defn wakes?
  [event-text]
  (clojure.string/includes? event-text "wakes up"))

(defn sleeping-events
  [[id entries]]
  (let [sleep-entries (filter #(or (asleep? (-> % :event :text))
                                   (wakes? (-> % :event :text)))
                              entries)]
    [id sleep-entries]))

(defn sleep-time
  [sleep-event wake-event]
  (let [{start-minute :minute} sleep-event
        {end-minute :minute} wake-event]
    (- end-minute start-minute)))

(defn guard-sleep-time
  [[id events]]
  (let [sleep-pairs (partition 2 events)
        times (map #(apply sleep-time %) sleep-pairs)]
    [id (reduce + times)]))

(def events (->> lines
                 (map Line->GuardEvent)
                 (sort)))

(def guardmaps (reduce-guards (take 10 events)))
(def sleeping-events (into {} (map sleeping-events guardmaps)))
(def sleeptimes (map guard-sleep-time sleeping-events))
