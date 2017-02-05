(ns coding-problems.4clojure
  (:gen-class)
  (:require [clojure.core.match :refer [match]])
  (:require [defun.core :refer [defun]]))


;#95

(fn [root]
    (every?
      #(or
        (nil? %)
        (and (sequential? %) (= 3 (count %))))
      (tree-seq sequential? rest root)))

(defn bn? [nodes]
  (or (nil? nodes)
      (when-let [children (and (coll? nodes) (rest (remove nil? nodes)))]
        (and (<= (count children) 2)
             (bn? (first children))
             (bn? (second children))))))

(fn bt? [t]
  (or (nil? t)
      (and (coll? t)
           (= (count t) 3)
           (bt? (nth t 1))
           (bt? (nth t 2)))))


;# 120 

((fn [s] (->> s
             (map #(re-seq #"\d" (str %)))
             (map #(map read-string %))
             (map #(reduce (fn [t x] (+ t (int (Math/pow x 2)))) 0 %))
             (keep-indexed (fn [i x] (if (< (nth s i) x) x)))
             count))
 (range 10))

(comp count
      (partial
       filter
       (fn [x]
         (< x (apply + (map (comp #(* % %) read-string str)
                            (str x)))))))

;; (fn func [aNumColl]
;;   (count (partial (filter (fn [x] (< x (reduce 
;;                                        (fn [aSSaum aCNum]
;;                                          (let [v (float (- (int aCNum) 48))]
;;                                            (+ aSSum (* v v)))) 
;;                                        0 (str x))))))))


; #128 Recognize Playing Cards


((fn [[s r]]
   (let [rm (merge (zipmap (map char (range 50 58)) (range 0 8))
                   {\T 8 \J 9 \Q 10 \K 11 \A 12})
         sm {\S :spade \H :heart \C :club \D :diamond}]
     (assoc {} :suit (sm s) :rank (rm r)))) "DQ")



(fn [[s r]]
  {:suit ({\D :diamond \H :heart \C :club \S :space} s)
   :rank ((zipmap "23456789TJQKA" (range)) r)})



(take  8)




(take 5 (#(iterate (partial + %) %) 3))

#(iterate (partial + %) %)

((fn [& xs] (first (apply clojure.set/intersection
                         (map (fn [x] (apply sorted-set (take 500 (iterate (partial + x) x))))
                              xs)))))



(fn [x & xs]
  (first
    (drop-while
      (fn [z] (some #(pos? (mod z %)) xs))
      (iterate #(+ x %) x))))



(fn lcm [& args]
  (letfn [(gcd [a b]
            (if (zero? b)
              a
              (gcd b (mod a b))))]
    (reduce #(/ (* % %2) (gcd % %2)) args)))





