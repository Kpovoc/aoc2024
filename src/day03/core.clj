(ns day03.core)

(def mul-fn "mul(")
(def max-num-size 3)

(defn is-num? [c]
  (let [v (Character/getNumericValue ^char c)]
    (and (>= v 0) (<= v 9))))

(defn parse-num [s term]
  (if (empty? s)
    {:num 0 :s s}
    (loop [rs s
           digits ""
           count 0]
      (if (empty? rs)
        {:num 0 :s (subs s 1)}
        (let [c (first rs)
              digit? (is-num? c)
              term? (= c term)]
          (cond
            (and (= count max-num-size) (not term?))
            {:num 0 :s (subs s 1)}

            (and (= count 0) (not digit?))
            {:num 0 :s (subs s 1)}

            (and (not term?) (not digit?))
            {:num 0 :s (subs s 1)}

            term?
            {:num (Integer/parseInt digits) :s (subs rs 1)}

            digit?
            (recur (subs rs 1) (str digits c) (inc count))))))))

(defn mult-args [s]
  (let [resp1 (parse-num s \,)
        num1 (:num resp1)]
    (if (= num1 0)
      {:num 0 :s (subs s 1)}
      (let [resp2 (parse-num (:s resp1) \))
            num2 (:num resp2)]
        (if (= num2 0)
          {:num 0 :s (subs s 1)}
          {:num (* num1 num2) :s (:s resp2)})))))

(defn parse-mult-func [s]
  (loop [mul mul-fn
         rs s
         nums []]
    (cond
      (empty? rs)
      (reduce + nums)

      :else
      (let [c (first rs)
            vc (first mul)]
        (cond
          (not= c vc)
          (if (= (count mul) 4)
            (recur mul-fn (subs rs 1) nums)
            (recur mul-fn rs nums))

          :else
          (if (= (count mul) 1)
            (let [res (mult-args (subs rs 1))]
              (recur mul-fn (:s res) (conj nums (:num res))))
            (recur (subs mul 1) (subs rs 1) nums)))))))

; count 0, then c is 0-9
; count 1, then c is 0-9 or ,
; count 2, then c is 0-9 or ,
; count 3, then c is ,

(defn -main []
  (println (parse-mult-func (slurp "data/day03"))))
