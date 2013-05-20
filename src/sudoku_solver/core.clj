(ns sudoku-solver.core
  (:use [clojure.contrib.math]))

(defn side-length [board]
  (exact-integer-sqrt (count board)))

(defn rows [board]
  (let [[row-size remaining] (side-length board)]
    (vec (map vec (partition row-size board)))))

(defn cols [board]
  (let [rs (rows board)]
    (vec (for [r (range (count rs))]
                 (vec (map (fn [v] (nth v r)) rs))))))

(defn square-offsets [outer inner]
  (map (fn [o]
         [(* inner (quot o inner)) (* inner (rem o inner))])
       (range outer)))

(defn squares [board]
  (let [rows (rows board)
        [row-size remaining] (side-length board)
        [inner-row-size _] (exact-integer-sqrt row-size)
        offsets (square-offsets row-size inner-row-size)]
    (vec (map #(let [[x y] %1]
                (vec (for [xo (range inner-row-size) yo (range inner-row-size)]
                  (get-in rows [(+ x xo) (+ y yo)]))))
              (square-offsets row-size inner-row-size)))))
