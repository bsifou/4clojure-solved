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



; #173 Intro to destructuring 2 :TODO:

(= 3
   (let [[op :as all] [+ (range 3)]] (apply op (rest all)))
   (let [[[:as all] b] [[+ 1] 2]] ((first all) (second all) b))
   (let [[:as all] [inc 2]] ((first all) (second all))))

;; issue in filling the blanks...



;; #147 Pascal's Trapezoid


(defn pas
  [xs]
  (iterate (fn [xs] (vec (map +' (cons 0 xs) (conj xs 0)))) xs)) 


(fn [x]
  (iterate (fn [v] (concat
                     [(first v)]
                     (map (partial apply +) (partition 2 1 v))
                     [(last v)]))
           x))


; #96  Beauty is Symmetry 


(fn sym?  [node]
  (let [rv (fn rv [node]
             (if (coll? node)
               (list (nth node 0)
                     (rv (nth node 2))
                     (rv (nth node 1)))
               node))]
    (and (coll? node)
       (= (rv (nth node 1))
          (nth node 2)))))


(fn [[v l r]]
  (= l
     ((fn mirror [[v l r :as t]]
        (when t [v (mirror r) (mirror l)])) r)))


(fn [[_ l r]] ((fn check [a b]
                (if (coll? a)
                  (let [[ak al ar] a
                        [bk bl br] b]
                    (and (= ak bk)
                         (check al br)
                         (check ar bl)))
                  (= a b))) l r))


; #146 Trees into tables

#(into {} (for [k1 (keys %)
               [k2 v] (get % k1)
               ;v (mapcat vals (vals m))
               ]
           [[k1 k2] v]))


#(into {}
       (for [[k v] m
             [k2 v2] v]
         [[k k2] v2]))

; #153 Pairwise Disjoint Sets

#(apply distinct? (apply concat %))


;#46  Flipping out

(fn flip [f]
  (fn [a b]
    (f b a)))

#(fn [a b] (% b a))

;#44 Rotate Sequence

((fn [n coll]
   (let [x (mod n (count coll))]
     (concat (drop x coll) (take x coll)))) 2 [1 2 3 4 5])

#(let [n (count %2)] (take n (drop (mod % n) (cycle %2))))



; #43 Reverse Interleave 

(fn  rev-interleave [coll n]
  (loop [c n
         left coll
         res '()]
    (if (zero? c)
      (reverse res)
      (let [add (take-nth c left)
            left (remove (set add) left)
            res (conj res add)]
        (recur (dec c)
               left
               res)))))


 

#(apply map list (partition %2 %))


(fn r-interleave [s n]
  (let [shift-add (fn [s val]
                    (cons (conj (last s) val) (butlast s)))]
    (reverse (reduce shift-add (repeat n []) s))))

;; every function tells us a story :) 


; #50 split by type


(#(set (vals (group-by type %))) [1 :a 2 :b 3 :c])

; #55 Count Occurrences


((fn [s]
   (reduce (fn [m x]
             (if (m x)
               (update-in m [x] inc)
               (assoc m x 1))) {} s)) [1 1 2 3 2 1 1])



; shorter, use the default get paramater when looking up key in map
(reduce #(assoc %1 %2 (inc (%1 %2 0))) {}  [1 1 2 3 2 1 1])

