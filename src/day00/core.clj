(ns day00.core
  (:require [clojure.string :as string]))

(defn three-letter-digit [word]
  (if (< (count word) 3)
    nil
    (let [w (subs word 0 3)]
      (cond
        (= w "one") 1
        (= w "two") 2
        (= w "six") 6
        :else nil))))

(defn four-letter-digit [word]
  (if (< (count word) 4)
    nil
    (let [w (subs word 0 4)]
      (cond
        (= w "four") 4
        (= w "five") 5
        (= w "nine") 9
        :else nil))))

(defn five-letter-digit [word]
  (if (< (count word) 5)
    nil
    (let [w (subs word 0 5)]
      (cond
        (= w "three") 3
        (= w "seven") 7
        (= w "eight") 8
        :else nil))))

(defn spelled-num [rest-line]
  (let [tld (three-letter-digit rest-line)
        fold (four-letter-digit rest-line)
        fild (five-letter-digit rest-line)]
    (cond
      (not (nil? tld)) tld
      (not (nil? fold)) fold
      (not (nil? fild)) fild
      :else nil)))

(defn to-int [^Character c] (Character/digit c 10))

(defn get-nums [line]
  (reduce #(if (> (to-int %2) -1) (str %1 %2) %1) "" (seq line)))

(defn reduce-digits [numStr]
  (if (empty? numStr)
    0
    (Integer/parseInt (str (first numStr) (last numStr)))))

(defn get-line-calibration [line]
  (-> line
      get-nums
      reduce-digits))

(defn sum-line [rt line]
  (+ rt (get-line-calibration line)))

(defn sum-lines [lines]
  (reduce sum-line 0 lines))

(defn -main []
  (println (sum-lines (string/split-lines (slurp "data/day00")))))
