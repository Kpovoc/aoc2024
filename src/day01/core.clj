(ns day01.core
  (:require [clojure.string :as string]))

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

(defn distance-diff [v1 v2]
  (let [sorted-v1 (sort v1)
        sorted-v2 (sort v2)]
    (reduce +
            (map #(Math/abs ^int %)
                 (map -
                      sorted-v1
                      sorted-v2)))))

(defn get-freq-value [n freq]
  (let [v (freq n)]
    (if (nil? v) 0 v)))

(defn similarity-score [v1 v2]
  (let [freq (frequencies v2)]
    (reduce + (map #(* % (get-freq-value % freq)) v1))))

(defn -main []
  (let [vs (read-in-lists "data/day01")
        v1 (first vs)
        v2 (second vs)]
    (println (distance-diff v1 v2))
    (println (similarity-score v1 v2))))