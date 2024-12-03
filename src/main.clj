(ns main
  (:require
   [clojure.string :as str]))

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
