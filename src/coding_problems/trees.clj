(ns coding-problems.trees
  (:gen-class)
  (:require [clojure.core.match :refer [match]])
  (:require [defun.core :refer [defun]]))


;; (definterface INode
;;   (getCar [])
;;   (setCar [x])
;;   (getCdr [])
;;   (setCdr [n])
;;   (reverse []))

;; (deftype Node
;;   [^:volatile-mutable car ^:volatile-mutable ^INode cdr]
;;   INode
;;   (getCar [_] car)
;;   (setCar [_ x] (set! car x))
;;   (getCdr [_] cdr)
;;   (setCdr [_ n] (set! cdr n))
;;   (reverse [this]
;;     (loop [cur this new-head nil]
;;       (if-not cur
;;         (or new-head this)
;;         (recur (.getCdr cur) (Node. (.getCar cur) new-head)))))

;;   clojure.lang.Seqable
;;   (seq [this]
;;     (loop [cur this acc ()]
;;       (if-not cur
;;         acc
;;         (recur (.getCdr cur) (conj acc (list (.getCar cur))))))))

(definterface INode
  (getLeft [])
  (getRight [])
  (setLeft [n])
  (setRight [n])
  (getKey [])
  (setKey [k])
  (getVal [])
  (setVal [v])
  (insert [k v])
  (lookup [k])
  (delete [k])
  (delete [k n]) ;; delete parent of that node
  (inOrder []))





;; comparator helpers 
(def gt? (comp pos? compare))

(def lt? (comp pos? compare))


(deftype Node
    [^:volatile-mutable key
     ^:volatile-mutable val
     ^:volatile-mutable ^INode left
     ^:volatile-mutable ^INode right]
  
  
    INode
    (getKey [_] key)
    (setKey [_] (set! key k))

    (getVal [_] val)

    (setVal [_ v] (set! val v))

    (delete [this k]
      (.delete this k nil))
    
    (getLeft [_] left)
    (getRight [_] right)
    (insert [this k v]
      (let [n (Node. k v nil nil)]
        (cond
          (gt? key k)  (if right
                         (.insert right k v)
                         (set! right n))
          (lt? key k) (if left
                        (.insert left k v)
                        (set! left n)))))

    (lookup [this k]
      ;;
      (if (= k key)
        val
        (cond
          (and (gt? key k) right) (.lookup right k)
          (and (lt? key k) left) (.lookup left k))))

    (inOrder [this]
      (lazy-cat
       (when left
         (.inOrder left))

       (vector val)
       
       (when right
         (.inOrder right))))


    (delete [this k parent]
       (letfn [;; a closure to help us set nodes on the parent node
            (set-on-parent [n]
              (if (identical? (.getLeft parent) this)
                (.setLeft parent n)
                (.setRight parent n)))

            ;; a function that finds the largest node in the
            ;; left subtree
            (largest [n]
              (let [right (.getRight n)]
                (when (.getRight right)
                  (largest right))
                right))]

      ;; if we have the target key, we fall into one of three
      ;; conditions
      (if (= k key)
        ;; note that the cond ordering is to ensure that we do
        ;; not match cases such as (or left right) before we
        ;; check (and left right)
        (cond
          ;; 3. two children, the most complex case: here we
          ;;    want to find either the in-order predecessor or
          ;;    successor node and replace the deleted node's
          ;;    value with its value, then clean it up
          (and left right) (let [pred (largest (.getLeft this))]
                             ;; replace the target deletion node
                             ;; with its predecessor
                             (.setKey this (.getKey pred))
                             (.setVal this (.getVal pred))

                             ;; set the deletion key on the
                             ;; predecessor and delete it as a
                             ;; simpler case
                             (.setKey pred k)
                             (.delete this k))

          ;; 1. no children, so we can simply remove the node
          (and (not left) (not right)) (set-on-parent nil)

          ;; 2. one child, so we can simply replace the old node
          ;;    with it
          :else (set-on-parent (or left right)))

        ;; otherwise we recurse, much like `lookup`
        (cond
          ;; if we have both a non-nil right node and `k` is
          ;; greater than key
          (and (gt? k key) right) (.delete right k this)

          ;; if we have both a non-nil left node and `k` is less
          ;; than key
          (and (lt? k key) left) (.delete left k this))))))






      










