(ns coding-problems.core
  (:gen-class)
  (:require [clojure.core.match :refer [match]])
  (:require [defun.core :refer [defun]]))


;; (definterface INode
;;   (getCar [])
;;   (setCar [k])
;;   (getCdr [])
;;   (setCdr [])
;;   (getKey [])
;;   (setKey [k])
;;   (getVal [])
;;   (setVal [v])
;;   (getNext [])
;;   (setNext [n]))

(definterface INode
  (getKey [])
  (setKey [k])
  (getVal [])
  (setVal [v])
  (getNext [])
  (setNext [n]))


(deftype Node
    [^:volatile-mutable key
     ^:volatile-mutable val
     ^:volatile-mutable next]
  INode
  (getKey [this] key)
  (setKey [this k] (set! key k))
  (getVal [this] val)
  (setVal [this v] (set! val v))
  (getNext [this] next)
  (setNext [this n] (set! next n)))

(definterface IHashTable
  (calcSize [])
  (maybeResize [])
  (bucketIdx [k])
  (setBucket [k v])
  (insert [k v])
  (insert [k v resize? ])
  (getBucket [k])
  (lookup [k])
  (findNode [k]))


;; (deftype HashTable
;;     [buckets size]
;;     IHashTable
;;     (bucketIdx [_ k] (mod (hash k) size))
;;     (setBucket [this k v] (aset buckets (.bucketIdx this k) v))
;;     (getBucket [this k] (aget buckets (.bucketIdx this k)))
;;     ;; (insert [this k v]
;;     ;;   (.setBucket this k (Node. k v nil)))
;;     (insert [this k v]
;;       (let [[prev node] (.findOne this k)]
;;         (cond
;;           (and prev (nil? node)) (.setNext prev (Node. k v nil))
;;           node (.setVal node v)
;;           :else (.setBucket this k (Node. k v nil)))))
;;     (lookup [this k]
;;       (when-let [[_ node] (.findNode this k)]
;;         (when node
;;           (.getVal node)))))


;(aset buckets (.bucketIdx this k) v)

;;--------------------


;(< occupancy (/ (* size load-factor) 2)) (/ size 2)



;;---------------------


;; (def hash-table
;;   (let [size 4
;;         load-factor 7/10]
;;     (HashTable. (make-array INode size) 0  size load-factor)))


;; (.insert hash-table "foo" "bar")


;; (.lookup hash-table "foo")



;; (deftype Node
;;   [^:volatile-mutable car ^:volatile-mutable ^INode cdr]
;;   INode
;;   (getCar [_] car)
;;   (setCar [_ x] (set! car x))
;;   (getCdr [_] cdr)
;;   (setCdr [this n] (set! cdr n) this)
;;   (reverse [this]
;;     (loop [cur this new-list nil]
;;       (if-not cur
;;         (or new-list this)
;;         (recur (.getCdr cur) (Node. (.getCar cur)  new-list)))))

;;   clojure.lang.Seqable
;;   (seq [this]
;;     (loop [cur this acc ()]
;;       (if-not cur
;;         acc
;;         (recur (.getCdr cur) (concat acc (list (.getCar cur))))))))


(seq (let [f "foo"] 
       (reify clojure.lang.Seqable 
         (seq [this] (list "dsf")))))



;; new implementation 

(definterface IHashTable
  (calcSize [])
  (maybeResize [])
  (bucketIdx [k])
  (getBucket [k])
  (setBucket [k v])
  (findNode [k])
  (insert [k v resize?])
  (insert [k v])
  (lookup [k])
  (delete [k]))

(deftype HashTable
    [^:volatile-mutable ^objects buckets
     ^:volatile-mutable ^long occupancy
     ^:volatile-mutable size
     ^float load-factor]
    IHashTable
    (calcSize [_]
      (when (-> occupancy zero? not)
        (cond
          ;; shrink
          (< occupancy (/ (* size load-factor) 2)) (/ size 2)
          ;; grow
          (> (/ occupancy size) load-factor) (* size 2))))

    (maybeResize [this]
      (when-let [new-size (.calcSize this)]
        (let [old-buckets buckets]
          (set! size new-size)
          (set! buckets (make-array INode new-size))

          ;; Loop over all old buckets. For any buckets containing
          ;; linked list nodes, walk these nodes. Extract their
          ;; keys and values, inserting them into the new buckets
          ;; array without triggering a call to maybeResize.
          (dotimes [i (alength old-buckets)]
            (if-let [bucket (aget old-buckets i)]
              (loop [^INode node bucket]
                (when node
                  (do (.insert this (.getKey node)
                               (.getVal node)
                               false)
                      (recur (.getNext node))))))))))

    (bucketIdx [_ k] (mod (hash k) size))

    (getBucket [this k] (aget buckets (.bucketIdx this k)))

    (setBucket [this k v] (aset buckets (.bucketIdx this k) v))

    (findNode [this k]
      (when-let [bucket (.getBucket this k)]
        (loop [^INode node bucket prev nil]
          (if (or (nil? node) (= (.getKey node) k))
            (vector prev node)
            (recur (.getNext node) node)))))

    (insert [this k v resize?]
      (let [[^INode prev ^INode node] (.findNode this k)]
        (cond
          ;; 1. bucket contains a linked list but not our key,
          ;;    set the next node
          (and prev (nil? node)) (.setNext prev (Node. k v nil))

          ;; 2. bucket contains a linked list and our key, reset 
          ;;    value
          node (.setVal node v)

          ;; 3. bucket is empty, create a new node and set the 
          ;;    bucket
          :else (do (.setBucket this k (Node. k v nil))
                    (set! occupancy (inc occupancy)))))
      (when resize? (.maybeResize this)))

    (insert [this k v] (.insert this k v true))

    (lookup [this k]
      (let [[_ ^INode node] (.findNode this k)]
        (when node
          (.getVal node))))

    (delete [this k]
      (let [[^INode prev ^INode node] (.findNode this k)]
        (cond
          ;; 1. non-head node contains the key, update its
          ;;    neighbors
          (and prev node) (.setNext prev (.getNext node))

          ;; 2. head node contains the key, update the head
          node (.setBucket this k (.getNext node))))
      (.maybeResize this)))


(defn hash-table [& [size load-factor]]
  (let [size (or size 1)
        load-factor (or load-factor 7/10)]
    (HashTable. (make-array INode size) 0 size load-factor)))


(def ^IHashTable my-hash-table (hash-table))

;(def my-hash-able (hash-table 1 7/10))

(.insert my-hash-table "foo" "bar")
(.insert my-hash-table "bar" "baz")
(.insert my-hash-table "baz" "qux")

(.lookup my-hash-table "foo")

(.insert my-hash-table "foo" "bar")





