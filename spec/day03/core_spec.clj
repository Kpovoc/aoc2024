(ns day03.core-spec
  (:require [speclj.core :refer :all]
            [day03.core :refer :all]))

(describe
  "parse-num"
  (it
    "returns 0 for the number, and the rest of the string from the starting character when invalid"
    (should= {:num 0 :s "*"} (parse-num "4*" \,))
    (should= {:num 0 :s ",9!"} (parse-num "6,9!" \)))
    (should= {:num 0 :s "2,345]"} (parse-num "12,345]" \)))
    (should= {:num 0 :s "2"} (parse-num "22" \)))
    (should= {:num 0 :s "234"} (parse-num "1234" \)))
    (should= {:num 0 :s "2 , 4 )"} (parse-num " 2 , 4 )" \,))
    (should= {:num 0 :s ""} (parse-num "" \,))
    (should= {:num 0 :s ""} (parse-num " " \,)))
  (it
    "returns number and rest of string after terminator"
    (should= {:num 2 :s "4)%&mul[3,7]!"} (parse-num "2,4)%&mul[3,7]!" \,))
    (should= {:num 43 :s "%&mul[3,7]!"} (parse-num "43)%&mul[3,7]!" \)))))

(describe
  "mult-args"
  (it
    "returns 0 for the number, and the rest of the string from the starting character when invalid"
    (should= {:num 0 :s ",7]!@^do_not"} (mult-args "3,7]!@^do_not"))
    (should= {:num 0 :s ")+mul"} (mult-args "5)+mul")))
  (it
    "returns the product and the rest of the string from the closing parenthesis"
    (should= {:num 8 :s "%&mul["} (mult-args "2,4)%&mul["))
    (should= {:num 25 :s "+mul(32,64]then("} (mult-args "5,5)+mul(32,64]then("))))

(describe
  "parse-mult-func"
  (it "sums the products of the mul(x,y) functions"
      (should= 161 (parse-mult-func "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"))))