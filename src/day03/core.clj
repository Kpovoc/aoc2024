(ns day03.core)

(def ul-fn "ul(")
(def o-fn "o()")
(def ont-fn "on't()")
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

(defn parse-mult-func
  "Assumes the m was already found"
  [s]
  (loop [mul ul-fn
         rs s]
    (cond
      (empty? rs)
      {:num 0 :s rs}

      :else
      (let [c (first rs)
            vc (first mul)]
        (cond
          (not= c vc)
          {:num 0 :s s}

          :else
          (if (= (count mul) 1)
            (mult-args (subs rs 1))
            (recur (subs mul 1) (subs rs 1))))))))

(defn parse-do-funcs
  "Assumes the d was already found"
  [s do?]
  (cond
    (empty? s)
    {:do? do? :s s}

    (and (>= (count s) 3) (= (subs s 0 3) o-fn))
    {:do? true :s s}

    (and (>= (count s) 6) (= (subs s 0 6) ont-fn))
    {:do? false :s s}

    :else
    {:do? do? :s s}))

(defn parse-for-funcs [s]
  (loop [rs s
         do? true
         nums []]
    (cond
      (empty? rs)
      (reduce + nums)

      :else
      (let [c (first rs)]
        (cond
          (and (= c \m) do?)
          (let [res (parse-mult-func (subs rs 1))
                num (:num res)]
            (if (= num 0)
              (recur (:s res) do? nums)
              (recur (:s res) do? (conj nums num))))

          (= c \d)
          (let [res (parse-do-funcs (subs rs 1) do?)]
            (recur (:s res) (:do? res) nums))
          :else
          (recur (subs rs 1) do? nums))))))

(defn -main []
  (println (parse-for-funcs (slurp "data/day03"))))
