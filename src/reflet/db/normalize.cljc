(ns reflet.db.normalize
  "Provides data normalization functions."
  (:refer-clojure :exclude [refer])
  (:require [clojure.spec.alpha :as s]
            [reflet.util.transients :as t]))

(defn ref?
  "Returns true if x is an entity reference."
  [x id-attrs]
  (and (vector? x)
       (= (count x) 2)
       (contains? id-attrs (first x))))

(defn ref-meta
  "Returns metadata for entity reference"
  [ref]
  (when (sequential? ref)
    (meta (second ref))))

(defn uuid-str [ref]
  (when (sequential? ref)
    (str (second ref))))

(defn many?
  "Returns true if x is a cardinality many value."
  [x id-attrs]
  (or (set? x)
      (and (sequential? x)
           (not (ref? x id-attrs)))))

(defn refer-one
  "Returns an entity reference to x, if x is a referable entity, nil
  otherwise."
  [x {:keys [refer-fn id-attrs]
      :or   {refer-fn find}}]
  (if (ref? x id-attrs)
    x
    (when (and (map? x) (not (sorted? x)))
      (some (partial refer-fn x) id-attrs))))

(defn- assert-kind
  [xs refs]
  (when-not (= (count refs) (count xs))
    (throw
     (ex-info
      "Cannot mix entities and non-entities"
      {:reason ::mixed-values
       ::value xs})))
  refs)

(defn refer-many
  "Same semantics as `refer-one`, but for cardinality many args. Mixing
  entities or entity references with non-entities is an error, and
  checking this condition necessarily realizes entire input sequence."
  [xs {:keys [id-attrs] :as opts}]
  (when (many? xs id-attrs)
    (some->> xs
             (keep #(refer-one % opts))
             (seq)
             (assert-kind xs))))

(defn like
  "Returns a collection of the same type as coll containing all the
  elements of xs."
  [coll xs]
  (cond
    (vector? coll) (vec xs)
    (set? coll)    (into (empty coll) xs) ; handles sorted-set
    :else          xs))

(defn normalize
  "Given a possibly nested entity, returns tx seq of normalized
  entities. Optionally applies transformation fn to all entities
  before normalization."
  [x {:keys [id-attrs transform-fn] :as opts}]
  (letfn [(transform [x]
            (if (and transform-fn (map? x))
              (transform-fn x)
              x))

          (normalize* [x]
            (when (map? x)
              (loop [[pair & more] x
                     tx            []
                     normalized    {}]
                (if-let [[k v] pair]
                  (if (many? v id-attrs)
                    (let [v*   (map transform v)
                          refs (refer-many v* opts)]
                      (if refs
                        (recur more
                               (into tx v*)
                               (->> (like v refs)
                                    (assoc normalized k)))
                        (recur more
                               tx
                               (assoc normalized k v))))
                    (let [v*  (transform v)
                          ref (refer-one v* opts)]
                      (if ref
                        (recur more
                               (conj tx v*)
                               (assoc normalized k ref))
                        (recur more
                               tx
                               (assoc normalized k v)))))
                  (->> tx
                       (keep normalize*)
                       (apply concat)
                       (cons normalized))))))]
    (if (map? x)
      (normalize* (transform x))
      [x])))

(defn to-many
  "Coerces x to a cardinality many value."
  [x id-attrs]
  (if (many? x id-attrs) x [x]))
