(ns day01.core-spec
  (:require [speclj.core :refer :all]
            [day01.core :refer :all]))

(describe "day01 problem 1 test"
  (it "Sorts vectors ltg, takes the difference in each index, and sums them"
      (let [l1 [3 4 2 1 3 3]
            l2 [4 3 5 3 9 3]]
        (should= 11 (distance-diff l1 l2)))))

(describe
  "day01 problem 2 test"
  (it "Calculates a similarity score"
      (let [l1 [3 4 2 1 3 3]
            l2 [4 3 5 3 9 3]]
        (should= 31 (similarity-score l1 l2)))))

(describe
  "line-list-to-num-lists"
  (it "turns a list of lines into 2 lists of numbers"
      (let [lines ["3425   564"
                   "3   57"
                   "678   45321"]]
        (should= [[3425 3 678] [564 57 45321]]
                 (line-list-to-num-lists lines)))))