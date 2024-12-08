(ns day04.core
  (:require [clojure.string :as string]))

; coordinates are in (row, column) format
(def direction {:north '(-1 0)
                :north-east '(-1 1)
                :east '(0 1)
                :south-east '(1 1)
                :south '(1 0)
                :south-west '(1 -1)
                :west '(0 -1)
                :north-west '(-1 -1)})

(def directions (keys direction))

(defn calc-next-coor [cc dir]
  (let [modifier (direction dir)]
    (list (+ (first modifier) (first cc))
          (+ (second modifier) (second cc)))))

(defn get-next-char [data cur-coor dir]
  (let [next-coor (calc-next-coor cur-coor dir)
        nc-row (first next-coor)
        nc-col (first next-coor)]
    (if (or (< nc-row 0) (< nc-col 0))
      {:coor next-coor
       :char nil}
      {:coor next-coor
       :char (get (get data (first next-coor)) (second next-coor))})))

(defn get-three-letter-x [data cur-coor c]
  (let [n (:char (get-next-char data cur-coor :north-east))
        s (:char (get-next-char data cur-coor :south-west))
        w (:char (get-next-char data cur-coor :north-west))
        e (:char (get-next-char data cur-coor :south-east))
        h1 (str e c w)
        h2 (str w c e)
        v1 (str n c s)
        v2 (str s c n)]
    [(str h1 v1)
     (str h1 v2)
     (str h2 v1)
     (str h2 v2)]))

(defn get-string-in-direction [data cur-coor dir str-len]
  (let [row (get data (first cur-coor))
        first-char (get row (second cur-coor))]
    (loop [s (str "" first-char)
           co cur-coor
           i 1]
      (if (>= i str-len)
        s
        (let [res (get-next-char data co dir)]
          (recur (str s (:char res)) (:coor res) (inc i)))))))

(defn get-strings-in-all-directions [data str-len cur-coor]
  (vec (map #(get-string-in-direction data cur-coor % str-len) directions)))

(defn get-indices-of-char [s c row]
  (loop [i 0
         indices []]
    (if (>= i (count s))
      indices
      (if (= c (get s i))
        (recur (inc i) (conj indices (list row i)))
        (recur (inc i) indices)))))

(defn get-locations-of-char [data c]
  (loop [i 0
         indices []]
    (if (>= i (count data))
      indices
      (recur (inc i)
             (into indices (get-indices-of-char (get data i) c i))))))

(defn count-found [data word]
  (let [c (first word)
        locs (get-locations-of-char data c)
        words (reduce into
                      []
                      (map #(get-strings-in-all-directions data (count word) %) locs))]
    (count (filter #(= word %) words))))

(defn count-x-mas [data]
  (let [locs (get-locations-of-char data \A)
        words (reduce into
                      []
                      (map #(get-three-letter-x data % \A) locs))]
    (count (filter #(= "MASMAS" %) words))))

(defn -main []
  (let [data (string/split-lines (slurp "data/day04"))]
    (println (count-found data "XMAS"))
    (println (count-x-mas data))))