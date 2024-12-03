(ns day02.core
  (:require [clojure.string :as string]))

(defn get-min-max [dif]
  (if (> dif 0)
    {:min 1 :max 3}
    {:min -3 :max -1}))

(defn get-next-dif [level]
  (- (first level) (second level)))

(defn is-not-in-range? [n min-val max-val damp?]
  (let [out-of-range? (or (< n min-val) (> n max-val))]
    (if out-of-range?
      (if damp?
        {:safe? true :damp? false}
        {:safe? false :damp? damp?})
      {:safe? true :damp? damp?}))
  )

(defn initial-level-safety [dif damp?]
  (if (= dif 0)
    {:damp? false
     :safe? damp?
     :min nil
     :max nil}
    (let [rng (get-min-max dif)
          safety (is-not-in-range? dif (:min rng) (:max rng) damp?)]
      {:damp? (:damp? safety)
       :safe? (:safe? safety)
       :min (:min rng)
       :max (:max rng)})))

(defn continue-level-check [init-level init-min-val init-max-val init-damp?]
  (loop [level init-level
         min-val init-min-val
         max-val init-max-val
         damp? init-damp?]
    (if (= (count level) 1)
      1
      (let [dif (get-next-dif level)]
        (if (nil? max-val)
          (let [safety (initial-level-safety dif damp?)]
            (if (not (:safe? safety))
              0
              (recur (rest level) (:min safety) (:max safety) (:damp? safety))))
          (let [safety (is-not-in-range? dif min-val max-val damp?)]
            (if (not (:safe? safety))
              0
              (recur (rest level) min-val max-val (:damp? safety)))))))))

(defn is-level-safe [level]
  (let [dif (get-next-dif level)
        safety (initial-level-safety dif false)]
    (if (:safe? safety)
      (continue-level-check (rest level) (:min safety) (:max safety) (:damp? safety)))
      0))

(defn determine-count-of-safe-levels [levels]
  (reduce + (map is-level-safe levels)))

(defn line-to-level [line]
  (map #(Integer/parseInt %) (string/split line #" ")))

(defn read-in-lines [s]
  (string/split-lines s))


(defn -main []
  (println
    (determine-count-of-safe-levels
      (map line-to-level
           (string/split-lines (slurp "data/day02"))))))
