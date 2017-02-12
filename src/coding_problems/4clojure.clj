(ns coding-problems.4clojure
  (:gen-class)
  (:require [clojure.core.match :refer [match]])
  (:require [defun.core :refer [defun]])
 (:require [functions-as-patterns.core  :refer :all]))

;#954




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


; #56 Find Distinct items


(fn de-dup [s]
  (loop [left s
         bag #{}
         res (list)]
    (if-not left 
      (reverse res)
      (let [[x & xs] left]
        (if (bag x)
          (recur xs bag res)
          (recur xs (conj bag x) (conj res x)))))))



(defn de-dup [s]
  (loop [[x & xs] s
         bag #{}
         res []]
    (if-not x
      (remove nil? res)
      (recur xs (conj bag x) (conj res (if (bag x) nil x))))))


(reduce (fn [res x]
          (if (not-any? (partial = x) res)
            (conj res x)
            res)) [] [1 2 1 3 1 2 4])


(fn [s] (remove nil? (map #(if (%2 %1) nil %1) s (reductions conj #{} s))))

;# 58 Function Composition

(fn comp-1
  ([f] f)
  ([f1 f2] (fn [& args] (f1 (apply f2 args))))
  ;([f1 f2 f3] (comp-1 (comp-1 f1 f2) f3))
  ([f1 f2 & fs] (reduce (fn [lf f] (comp-1 lf f)) (comp-1 f1 f2) fs)))



(fn [& fs]
  (let [[rf & rfs] (reverse fs)]
    (fn [& args]
      (reduce #(%2 %) (apply rf args) rfs))))

; #54 Partition a Sequence


(fn part [n s]
  (loop [s s
         r []]
    (if (>= (count s) n)
      (recur (drop n s) (conj r (take n s)))
      r)))

((fn part-2 [n coll]
  (when-let [s (seq coll)]
    (let [p (take n s)]
      (when (= (count p) n)
        (cons p (part-2 n (nthrest s n))))))) 3 (range 9))

;# 59 Juxtaposition

(fn jxt
  [& fns]
  (fn [& args]
    (map #(apply %1 args) fns)))

(fn jxt-2
  [& fns]
  (fn [& args]
    (reduce #(conj %1 (apply %2 args)) [] fns)))

;# 



(defn seive
  [n]
  (let [s (map vector (range 2 (inc n)) (vec (repeat (dec n) true)))]
    (->> (loop [[[i b] & xs] s
                r s]
           (if (and xs (<= i (Math/sqrt n))) 
             (recur
              xs
              (if b
                (let [set
                      (set
                       (for [c (range)
                             :let [j (+ (* i i) (* i c))]
                             :while (<= j n)]
                         j))]
                  (map (fn [[j p?]]
                         (if (set j) [j false] [j p?])) r))
                r))
             r))
         (filter second)
         (map first))))

;; :TODO: make github gist for my implementation of the seive algorithm
;; and check other implemntation pereferbably form kohyama! 

; usin core async! https://github.com/clojure/core.async/wiki/Sieve-of-Eratosthenes



(defn primes< [n]
  (if (<= n 2)
    ()
    (remove (into #{}
                  (mapcat #(range (* % %) n %))
                  (range 3 (Math/sqrt n) 2))
            (cons 2 (range 3 n 2)))))

(defn primes-to
  "Computes lazy sequence of prime numbers up to a given number using sieve of Eratosthenes"
  [n]
  (letfn [(nxtprm [cs]                  ; current candidates
            (let [p (first cs)]
              (if (> p (Math/sqrt n)) cs
                  (cons p (lazy-seq (nxtprm (-> (range (* p p) (inc n) p)
                                                set (remove cs) rest)))))))]
    (nxtprm (range 2 (inc n)))))



(defn primes-to
  "Computes lazy sequence of prime numbers up to a given number using sieve of Eratosthenes"
  [n]
  (let [root (-> n Math/sqrt long),
        cmpsts (boolean-array (inc n)),
        cullp (fn [p]
                (loop [i (* p p)]
                  (if (<= i n)
                    (do (aset cmpsts i true)
                        (recur (+ i p))))))]
    (do (dorun (map #(cullp %) (filter #(not (aget cmpsts %))
                                       (range 2 (inc root)))))
        (filter #(not (aget cmpsts %)) (range 2 (inc n))))))


(defn primes-to
  "Returns a lazy sequence of prime numbers less than lim"
  [lim]
  (let [refs (boolean-array (+ lim 1) true)
        root (int (Math/sqrt lim))]
    (do (doseq [i (range 2 lim)
                :while (<= i root)
                :when (aget refs i)]
          (doseq [j (range (* i i) lim i)]
            (aset refs j false)))
        (filter #(aget refs %) (range 2 lim)))))

;todo 
(defn primes-to
  "Returns a lazy sequence of prime numbers less than lim"
  [lim]
  (let [max-i (int (/ (- lim 1) 2))
        refs (boolean-array max-i true)
        root (/ (dec (int (Math/sqrt lim))) 2)]
    (do (doseq [i (range 1 (inc root))
                :when (aget refs i)]
          (doseq [j (range (* (+ i i) (inc i)) max-i (+ i i 1))]
            (aset refs j false)))
        (cons 2 (map #(+ % % 1) (filter #(aget refs %) (range 1 max-i)))))))


;; # 67 prime numbers


((fn [n] (take n ((fn primes
                   ([] (cons 2 (primes 3)))
                   ([x]
                    (if (some #(zero? (mod x %)) (range 2 (inc (int (Math/sqrt x)))))
                      (lazy-seq (primes (inc x)))
                      (cons x (lazy-seq (primes (inc x)))))))))) 2)

((fn [n]
   (take n
         (filter
          (fn [m] (not-any? zero? (map #(mod m %) (range 2 m))))
          (iterate inc 2)))) 5)

;#65 Black Box Testing

;; TODO, clean up the redundancies!

(fn t? [s]
  (cond
    (= s (into #{} s)) :set
    (and (associative? s)
         (not= (count (flatten (assoc s 0 1))) (count (assoc s 0 1)))) :map 
    (and (= s (into [] s))
         (associative? s)
         (= (last (conj s 'spc)) 'spc)) :vector
    (and (= (reverse s) (into () s))
         (= (first (conj s 'spc))  'spc))  :list))


#(cond
  (= (count (conj % [0 0]))
     (count (conj % [0 0] [0 1]))) :map
  (= (conj % 0) (conj % 0 0)) :set
  (= (next (conj % 0 1)) (conj % 0)) :list
  :else :vector)


(fn tpe [obj]
  (let [v [:test :result]
        obj (conj obj v)]
    (cond (:test obj)
          :map
          (= (conj obj v) obj)
          :set
          (= (last (conj obj :aaaa)) :aaaa)
          :vector
          :default :list)))

;#74 filter perfect squares 

((fn [s]
   (apply str (interpose "," 
                         (map str (filter (fn [x] (let [root-i (int (Math/sqrt x))]
                                                   (== (* root-i root-i) x)))))))) )

((fn [s] (->> 
         (clojure.string/split s  #",")
         (map read-string)
          (filter (fn [x] (let [root-i (int (Math/sqrt x))]
                           (== (* root-i root-i) x))))
         (map str)
         (interpose ",")
         (apply str))) "4,5,6,7,8,9")





;; (postwalk-demo [:a :b [:c :d [:e] :f]])
;; :a
;; :b
;; :c
;; :d
;; :e
;; [:e]
;; :f
;; [:c :d [:e] :f]
;; [:a :b [:c :d [:e] :f]]



;; (prewalk-demo [:a :b [:c :d [:e] :f]])

;; [:a :b [:c :d [:e] :f]]
;; :a
;; :b
;; [:c :d [:e] :f]
;; :c
;; :d
;; [:e]
;; :f


; # 76 Intro to Trampline 


(= [1 3 5 7 9 11]
 (letfn
     [(foo [x y] #(bar (conj x y) y))
      (bar [x y] (if (> (last x) 10)
                   x
                   #(foo x (+ 2 y))))]
     (trampoline foo [] 1)))


;; #77 Anagram Finder

(fn ana [s]
  (loop [[f & xs] (seq s)
         set #{}]
    (if f
      (let [found (reduce (fn [r x] (if (and
                                        
                                        ;(every? #(clojure.string/includes? f (str %)) x)
                                        (= (frequencies x) (frequencies f)))
                                     (conj r x)
                                     r)) #{} xs)]
        (if (empty? found)
          (recur xs set)
          (recur (remove found xs) (conj set (conj found f)))))
      set)))

#(set (map set
           (remove (comp zero? dec  count)
                   (vals (group-by frequencies %)
                                        ;(group-by sort  ["meat" "mat" "team" "mate" "eat"])
                         ))))

;; 80 Perfect numbers


(fn [n]
  (= (apply + (filter #(zero? (mod n %)) (range 1 n))) n))

(fn reductions-
  ([f [x & xs]] (reductions- f x xs))
  ([f val [x & xs :as s]]
   (cons val (when x
               (let [new-val (f val x)]
                 (lazy-seq (reductions- f new-val xs)))))))



(defn reductions-
  ([f [x & xs]] (reductions- f x xs))
  ([f val [x & xs :as s]]
   (lazy-cat [val]
             (if x
               (let [new-val (f val x)]
                 (lazy-seq (reductions- f new-val xs)))))))


;(reductions- conj [1] [2 3 4])


; #69 Merge with a function 

(defn merge-with- [f & maps]
  (reduce (fn [map m] (reduce-kv (fn [map k v]
                                  (assoc map k
                                         (if-let [ov (get map k)]
                                           (f ov v)
                                           v))) map m))
          maps))



(defn mw [f m & [h & r]]
  (if h
    (recur f
           (reduce (fn [a [k v]] (assoc a k (if-let [av (a k)] (f av v) v))) m h)
           r)
    m))




;(merge-with2 - false {:k 3} {:k 1})


(defn merge-with2
  [f & maps]
  (when (some identity maps)
    (let [merge-entry (fn [m e]
			(let [k (key e) v (val e)]
			  (if (contains? m k)
			    (assoc m k (f (get m k) v))
			    (assoc m k v))))
          merge2 (fn [m1 m2]
		   (reduce merge-entry m1 (seq m2)))]
      (reduce merge2 maps))))

;# 102 intoCamelCase

(fn [s]
  (let [[x & ss] (clojure.string/split s #"-")]
    (apply str x (map clojure.string/capitalize ss))))


(fn [s]
  (->> s
       (re-seq #"\w+")
       (map-indexed #(if (zero? %1) %2 (clojure.string/capitalize %2)))
       (apply str)))

; #75 Eurer's Totient Function

(fn et [n]
  (letfn [(gcd [a b]
            (if (zero? b)
              a
              (gcd b (mod a b))))

          (co-p? [a b]
            (= (gcd a b) 1))]
    (if (= n 1)
      1
      (count (filter (partial co-p? n) (range 1 n))))))


(fn [n]
  (if (= n 1) 1
      (let [gcd (fn f [a b] (if (zero? b) a (f b (mod a b))))]
        (count (filter #(= 1 (gcd n %)) (range 1 n))))))


(defn totient [x]
  (let [prime? (fn [x]
                 (->> (range 2 x)
                      (map #(rem x %))
                      (not-any? zero?)))]
    (->> (range 2 (inc x))
         (filter (every-pred prime? #(zero? (rem x %))))
         (clojure.walk/walk #(- 1 (/ %)) #(apply * %))
         (* x)
         int)))

;(totient 10)


;# 86 Happy Numbers


;; (reduce-kv (fn [r k v]
;;              (if (= 1 k)
;;                (assoc r k (inc v))
;;                ;(conj r v)
;;                (assoc r k v)))
;;            []
;;            [0 0 2 3])

(-> [10 11]
    (conj 12)
    ;(as-> xs (map - xs [3 2 1]))
    )

;(use 'clojure.data)

; 86 Happy numbers 

;(* 7 7) -> 49 -> (+ (* 4 4) (* 9 9)) -> 97 (+ (* 9 9) (* 7 7)) -> 130 (+ 1 9 ) 10 -> 1


; dotimes, iterate, reduced

(fn happy? [n]
  (let [sum-sqr (fn sum-sqr [n]
                  (apply + (map (comp #(* % %) read-string  str) (str n))))
        limit 1000]
    (= (last (take limit (iterate (fn [x] (sum-sqr x)) n))) 1)))


(defn happy2? [n]
  (let [sum-sqr (fn sum-sqr [n]
                  (apply + (map (comp #(* % %) read-string  str) (str n))))]
    (first (drop-while
            ;vector?
            (complement (some-fn true? false?))
            (iterate (fn [[s x]]
                       (let [next (sum-sqr x)]
                         (cond 
                           (= x 1) true
                           (some (partial = x) s) false
                           :else [(conj s x) next]))) [[] n])))))


;(= 1 (nth (iterate (fn [n] (->> (str n) (map #(Character/digit % 10)) 


(defn happy3? [n]
  (letfn [(sum-sqr [n]
            (apply + (map (comp #(* % %) read-string  str) (str n))))]
      (loop [seen #{}
             x n]
        (cond
          (seen x) false
          (= x 1) true
          :else (recur (conj set x) (sum-sqr x))))))

;78 reimplement trampline

(fn tramp [f & args]
  (let [r (apply f args)]
   (if (fn? r)
     (recur r nil)
     r)))


(fn tramp2 [f & args]
  (loop [g (apply f args)]
    (if (fn? g) (recur (g)) g)))

(fn tramp3
  ([f]
   (let [ret (f)]
     (if (fn? ret)
       (recur ret)
       ret)))
  ([f & args]
   (trampoline #(apply f args))))


;; spped of sound 


;; The balance of N 115

(->> n
    (str)
    (split-at ))

(fn [coll]
  (let  [ds ((comp (partial map (comp read-string str)) str) coll)
        c (count ds)
        [r l]  (split-at (/ c 2) ds)]
     (= (apply + r) (apply + (if (odd? c) (last r) 0) l))))

(fn [n]
  (let [s (str n)
        m (quot (count s) 2)
        ds #(apply + (map (comp read-string str) (take m %)))]
    (= (ds s) (ds (reverse s)))))


;; 85 power set


(fn powerset [xset]
  (if (empty? xset)
    #{#{}}
    (let [xcompl (disj xset (first xset))
          powcompl (powerset xcompl)
          xadded (map #(conj % (first xset)) powcompl)]
      (clojure.set/union powcompl xadded))))


(fn powerset [xset]
  (if (empty? xset)
    #{#{}}
    (as->
        (disj xset (first xset)) s
        (powerset s)
        (clojure.set/union s (map #(conj % (first xset)) s)))))



#(reduce
  (fn [a x] (into a (map #(conj % x) a)))
  #{#{}} %)

;; 98 Equivalence classes


;; maybe use reduce?

(fn  [f xset]
  (reduce (fn [r x]
            (conj  r
                   (set (filter  #(= (f x) (f %)) (apply disj xset r)))))
          #{}
          xset))


(defn ec2 [f xset]
  (->> xset
       (group-by f)
       (vals)
       (map set)
       (set)))

(let [f (fn [x] (* x x))]
    (partition-by  #(= (f 1) (f %)) [-1 -2 1 2 3 4 5]))

;; 105 Idenitty keys and values 


;; juxt?


(fn transf [coll]
  (let [s (partition-all 2 (partition-by keyword? coll))
        f1 #(map (fn [[[& keys] vals]]
                   [(last keys)  (vec vals)])
                 %)
        f2 #(keep (fn [[[& keys] _]]
                    (if-let [ks (butlast keys)]
                      (map (fn [k] [k []]) ks)))
                  %)]
    (->> ((juxt f1 (comp first f2)) s)
        (apply concat)
        (into {} ))))


;(partition-all 2 (partition-by keyword? [:a 1 2 3 :b :c 4]))


(defn transf [s]
  (apply array-map
         (mapcat (fn [[k :as e]]
                   (if (keyword? k)
                     (interpose [] e)
                     [e]))
                 (partition-by keyword? s))))


(defn transf [v]
  (first (reduce (fn [[res last] v]
                   (if (number? v)
                     [(update res last conj v) last]
                     [(assoc res v []) v]))
                 [{} nil] v)))






(transf [:a 1 2 3 :b :c 4])





