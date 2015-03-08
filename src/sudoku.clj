(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (pos? (value-at board coord)))

(defn row-values [board coord]
  (let [[row _] coord]
    (set (get board row))))

(defn col-values [board coord]
  (let [[_ col] coord]
    (set (map #(get % col) board))))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]))

(defn block-values [board coord]
  (let [find-coord (fn [n]
                     (let [zero #{0 1 2}
                           three #{3 4 5}
                           six #{6 7 8}]
                       (cond (contains? zero n) 0
                             (contains? three n) 3
                             (contains? six n) 6
                             :else nil)))
        row (find-coord (get coord 0))
        col (find-coord (get coord 1))]
    (apply set/union (map (fn [n]
                            (let [row (get board n)]
                              (set (map #(get row %)
                                        (range col (+ col 3))))))
                          (range row (+ row 3))))))

(defn valid-values-for [board coord]
  (let [union set/union
        diff set/difference]
    (if (has-value? board coord) #{}
        (diff all-values
              (union (row-values board coord)
                     (col-values board coord)
                     (block-values board coord))))))

(defn filled? [board]
  (let [rows (map #(row-values board [% 0]) (range 9))
        vals (apply set/union rows)]
    (not (contains? vals 0))))

(defn rows [board]
  (for [x (range 0 9)]
    (row-values board [x 0])))

(defn valid-rows? [board]
  (every? #(= % all-values) (rows board)))

(defn cols [board]
  (for [y (range 0 9)]
    (col-values board [0 y])))

(defn valid-cols? [board]
  (every? #(= % all-values) (cols board)))

(defn blocks [board]
  (for [x [0 3 6] y [0 3 6]]
    (block-values board [x y])))

(defn valid-blocks? [board]
  (every? #(= % all-values) (blocks board)))

(defn valid-solution? [board]
  (and (valid-blocks? board)
       (valid-cols? board)
       (valid-rows? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [coords (for [x (range 0 9) y (range 0 9)]
                 [x y])]
    (first (filter #(not (has-value? board %)) coords))))

(defn solve-helper [board]
  (if (empty? (find-empty-point board))
    (if (valid-solution? board)
      [board]
      '())
    (let [point (find-empty-point board)
          values (valid-values-for board point)]
      (for [val values]
        (reduce concat '() (solve-helper (set-value-at board point val)))))))

(defn solve [board]
  (first (filterv (complement empty?) (solve-helper board))))
