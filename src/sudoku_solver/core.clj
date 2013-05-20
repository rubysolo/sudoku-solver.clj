(ns sudoku-solver.core
  (:use [clojure.contrib.math]))

(defn side-length [board]
  (exact-integer-sqrt (count board)))

(defn rows [board]
  (let [[row-size remaining] (side-length board)
        index-offset (fn [rindex] (* row-size rindex))]
    (map (fn [offset]
           (subvec board offset (+ row-size offset)))
      (map index-offset (range row-size)))))

(defn cols [board]
  (let [rs (rows board)]
    (vec (for [r (range (count rs))]
                 (vec (map (fn [v] (nth v r)) rs))))))

(defn square-offsets [outer inner]
    (map (fn [s]
           (+ (* outer inner (quot s inner))
              (* inner (rem s inner))))
         (range outer)))

(defn squares [board]
  (let [[row-size remaining] (side-length board)
        [inner-row-size _] (exact-integer-sqrt row-size)
        offsets (square-offsets row-size inner-row-size)]
    (vec (map (fn [o]
           (vec (flatten (map (fn [r]
             (let [offset (+ o (* row-size r))]
               (subvec board offset (+ inner-row-size offset))))
               (range inner-row-size)))))
         offsets))))
