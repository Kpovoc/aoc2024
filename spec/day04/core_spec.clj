(ns day04.core-spec
  (:require [speclj.core :refer :all]
            [day04.core :refer :all]))

(def test-group-letters
  ["abcde"
   "fghij"
   "klmno"
   "pqrst"
   "uvwxy"])

(describe "get-next-char"
          (it "gets the next character in a given direction from the current coordinate"
              (should= {:coor '(1 2) :char \h} (get-next-char test-group-letters '(2 2) :north))
              (should= {:coor '(1 3) :char \i} (get-next-char test-group-letters '(2 2) :north-east))
              (should= {:coor '(2 3) :char \n} (get-next-char test-group-letters '(2 2) :east))
              (should= {:coor '(3 3) :char \s} (get-next-char test-group-letters '(2 2) :south-east))
              (should= {:coor '(3 2) :char \r} (get-next-char test-group-letters '(2 2) :south))
              (should= {:coor '(3 1) :char \q} (get-next-char test-group-letters '(2 2) :south-west))
              (should= {:coor '(2 1) :char \l} (get-next-char test-group-letters '(2 2) :west))
              (should= {:coor '(1 1) :char \g} (get-next-char test-group-letters '(2 2) :north-west))))


(describe
  "get-string-in-direction"
  (it "gets a string of given length in the given direction"
      (should= "mhc" (get-string-in-direction test-group-letters '(2 2) :north 3))
      (should= "mie" (get-string-in-direction test-group-letters '(2 2) :north-east 3))
      (should= "mno" (get-string-in-direction test-group-letters '(2 2) :east 3))
      (should= "msy" (get-string-in-direction test-group-letters '(2 2) :south-east 3))
      (should= "mrw" (get-string-in-direction test-group-letters '(2 2) :south 3))
      (should= "mqu" (get-string-in-direction test-group-letters '(2 2) :south-west 3))
      (should= "mlk" (get-string-in-direction test-group-letters '(2 2) :west 3))
      (should= "mga" (get-string-in-direction test-group-letters '(2 2) :north-west 3))
      (should= "mh" (get-string-in-direction test-group-letters '(2 2) :north 2))))

(describe
  "get-strings-in-all-directions"
  (it "gets a list of strings in all directions of given length from current coords"
      (should= ["mhc" "mie" "mno" "msy"
                "mrw" "mqu" "mlk" "mga"]
               (get-strings-in-all-directions test-group-letters 3 '(2 2)))))