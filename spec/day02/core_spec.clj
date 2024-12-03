(ns day02.core-spec
  (:require [speclj.core :refer :all]
            [day02.core :refer :all]))

(describe "is-level-safe"
  (it "Is unsafe"
    (should= 1 1)))
