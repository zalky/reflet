(ns reflet.db.tx
  (:require [clojure.zip :as z]))

(defn- into-seq
  [coll]
  (cond
    (seq? coll) coll
    (nil? coll) coll
    :else       (or (seq coll) '())))

(defn- tx-branch?
  [x]
  (or (map? x)
      (sequential? x)
      (set? x)))

(defn- tx-children
  [x]
  (if (map? x)
    (vals x)
    (seq x)))

(defn- tx-node
  [p c]
  (let [n (cond
            (map? p) (zipmap (keys p) c)
            (seq? p) (into-seq c)
            :else    (into (empty p) c))]
    (with-meta n (meta p))))

(defn tx-zipper
  "Zipper for traversing transactions. Faster and more robust than
  clojure.walk, which does not play well with metadata."
  [tx]
  (z/zipper tx-branch?
            tx-children
            tx-node
            tx))

(defn walk-tx
  "Walks all nodes of the tx, applying `f` post depth-first traversal."
  [f tx]
  (let [loc (tx-zipper tx)]
    (if (z/branch? loc)
      (let [node (z/node loc)]
        (->> (z/children loc)
             (map (partial walk-tx f))
             (z/make-node loc node)
             (f)))
      (z/root loc))))

(defn walk-maps
  [f tx]
  (letfn [(xform [node]
            (cond-> node
              (map? node) f))]
    (walk-tx xform tx)))

(defn reduce-maps
  [f init tx]
  (let [acc (volatile! init)]
    (walk-maps #(vswap! acc f %) tx)
    @acc))
