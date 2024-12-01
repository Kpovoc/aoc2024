(ns day01.core
  (:require [clojure.string :as string]))

(defn distance-diff [l1 l2]
  (let [sorted-l1 (sort l1)
        sorted-l2 (sort l2)]
    (reduce +
            (map #(Math/abs ^int %)
                 (map -
                      sorted-l1
                      sorted-l2)))))

(defn line-to-num-pair [line]
  (map #(Integer/parseInt %) (string/split line #"   ")))

(defn line-list-to-num-lists [lines]
  (loop [r-lines lines
         v1 []
         v2 []]
    (if (empty? r-lines)
      [v1 v2]
      (let [nums (line-to-num-pair (first r-lines))]
        (recur (rest r-lines) (conj v1 (first nums)) (conj v2 (second nums)))))))

(defn read-in-lists [filename]
  (line-list-to-num-lists (string/split-lines (slurp filename))))

(defn calc-prob-1 [filename]
  (let [vs (read-in-lists filename)
        v1 (first vs)
        v2 (second vs)]
    (distance-diff v1 v2)))

(defn -main []
  (println (calc-prob-1 "data/day01")))