(ns main
  (:require
   [clojure.string :as str]
   [clojure.test :refer [with-test is]]))

;;; Day 01

;;;; Exercise 01

(def last-known-locations (slurp "data/01-last-known-locations"))

(defn parse-locs [s]
  (map Integer/parseInt (str/split s #"\s+")))

;; TODO: Validate the input.

(defn locs->loc-lists [locs]
  (let [take-by-odd-idx  (partial take-nth 2)
        take-by-even-idx (comp take-by-odd-idx rest)]
    ((juxt take-by-odd-idx take-by-even-idx) locs)))

(defn loc-lists->loc-pairs [loc-lists]
  (->> loc-lists
       (map sort)
       (apply map vector)))

(defn calc-total-distance [loc-pairs]
  (reduce (fn [acc loc-pair]
            (+ acc (apply - (sort > loc-pair))))
          0
          loc-pairs))

(comment
  ;; "Main" for day 01, exercise 01.
  (-> last-known-locations
      parse-locs
      locs->loc-lists
      loc-lists->loc-pairs
      calc-total-distance)
  ;; => 2769675
:end)

;;;; Exercise 02

(defn similarity-score [[loc-list-a loc-list-b :as _loc-lists]]
  (reduce + (for [loc loc-list-a]
              (* loc (count (filter #{loc} loc-list-b))))))

(comment
  ;; Answer for day 01, exercise 02.
  (-> last-known-locations
      parse-locs
      locs->loc-lists
      similarity-score)
  ;; => 24643097
  :end)


;;; Day 02

;;;; Exercise 01
(def unusual-data (slurp "data/02-unusual-data"))

(defn parse-reports [s]
  (into [] (comp
            (map #(str/split % #"\s+"))
            (map (partial mapv #(Integer/parseInt %))))
        (str/split s #"\n")))

;; The levels are all either increasing or all decreasing.

(defn gradual-transition? [report]
  (or (apply < report)
      (apply > report)))

;; Any two adjacent levels differ by at least one and at most three.

(defn safe-transition? [report]
  (->> report
       (partition 2 1)
       (map (partial sort >))
       (map (partial apply -))
       (every? #{1 2 3})))

(with-test

  (defn safe-report? [report]
    (and (gradual-transition? report)
         (safe-transition? report)))

  (is (true? (safe-report? [7 6 4 2 1]))
      "Safe because the levels are all decreasing by 1 or 2.")
  (is (false? (safe-report? [1 2 7 8 9]))
      "Unsafe because 2 7 is an increase of 5.")
  (is (false? (safe-report? [9 7 6 2 1]))
      "Unsafe because 6 2 is a decrease of 4.")
  (is (false? (safe-report? [1 3 2 4 5]))
      "Unsafe because 1 3 is increasing but 3 2 is decreasing.")
  (is (false? (safe-report? [8 6 4 4 1]))
      "Unsafe because 4 4 is neither an increase or a decrease.")
  (is (true? (safe-report? [1 3 6 7 9]))
      "Safe because the levels are all increasing by 1, 2, or 3."))

(comment
  ;; Solution to day 02, exercise 01.
  (->> unusual-data
       parse-reports
       (filter safe-report?)
       count)
  ;; => 202
  :end)

;;;; Exercise 02

(defn- drop-nth [index coll]
  (let [[a b] (split-at index coll)]
    (concat a (rest b))))

(with-test

  (defn safe-report?* [report]
    (let [reports (repeat (count report) report)]
      (->> reports
           (map-indexed drop-nth)
           (some safe-report?)
           boolean)))

  (is (true? (safe-report?* [7 6 4 2 1]))
      "Safe without removing any level.")
  (is (false? (safe-report?* [1 2 7 8 9]))
      "Unsafe regardless of which level is removed.")
  (is (false? (safe-report?* [9 7 6 2 1]))
      "Unsafe regardless of which level is removed.")
  (is (true? (safe-report?* [1 3 2 4 5]))
      "Safe by removing the second level, 3.")
  (is (true? (safe-report?* [8 6 4 4 1]))
      "Safe by removing the third level, 4.")
  (is (true? (safe-report?* [1 3 6 7 9]))
      "Safe without removing any level."))

(comment
  ;; Solution to day 02, exercise 02.
  (->> unusual-data
       parse-reports
       (filter safe-report?*)
       count)
  ;; => 271
  :end)

;;; Day 03

;;;; Exercise 01

(def corrupted-memory (slurp "data/03-corrupted-memory"))

(with-test

  (defn process-valid-instructions [memory]
    (->> memory
         (re-seq #"mul\((\d+{3})\,(\d+{3})\)")
         (map (fn [[_raw-instruction arg1 arg2]]
                (* (Integer/parseInt arg1) (Integer/parseInt arg2))))
         (reduce +)))

  (let [corrupted-memory-example "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"]
    (is (= 161 (process-valid-instructions corrupted-memory-example)))))

(comment
  ;; Solution to day 03, exercise 01.
  (process-valid-instructions corrupted-memory)
  ;; => 174960292
  :end)

;;;; Exercise 02

(with-test

  (defn process-valid-instructions* [memory]
    (process-valid-instructions
     (transduce (comp (map #(str/split % #"don't\(\)"))
                      (map first))
                str
                (str/split memory #"do\(\)"))))

  (let [corrupted-memory-example-2 "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"]
    (is (= 48 (process-valid-instructions* corrupted-memory-example-2)))))
.
(comment
  ;; Solution to day 03, exercise 02.
  (process-valid-instructions* corrupted-memory)
  ;; => 56275602
  :end)
