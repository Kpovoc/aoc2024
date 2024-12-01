(ns day00.core-spec
  (:require [speclj.core :refer :all]
            [day00.core :refer :all]))

(describe
  "spelled-num"
  (it "gets 8 from eight"
      (should= 8 (spelled-num "eight")))
  (it "gets 5 from fiveeight"
      (should= 5 (spelled-num "fiveeight")))
  )

(describe
  "get-line-calibration"
  (it "gets 12 from 1abc2"
      (should= 12 (get-line-calibration "1abc2")))
  (it "gets 38 from pqr3stu8vwx"
      (should= 38 (get-line-calibration "pqr3stu8vwx")))
  (it "gets 15 from a1b2c3d4e5f"
      (should= 15 (get-line-calibration "a1b2c3d4e5f")))
  (it "gets 77 from treb7uchet"
      (should= 77 (get-line-calibration "treb7uchet"))))

(describe
  "sum-lines"
  (it "sums the calibration of a single line"
      (should= 12 (sum-line 0 "1ab2c")))
  (it "sums the calibration result of all lines"
      (let [lines ["1abc2" "pqr3stu8vwx" "a1b2c3d4e5f" "treb7uchet"]]
        (should= 142 (sum-lines lines)))))
