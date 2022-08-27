(ns reflet.util
  (:require [clojure.set :as set]
            [clojure.spec.alpha :as s]
            [clojure.zip :as z])
  #?(:clj (:import java.lang.management.ManagementFactory)))

(defn seqify
  "Coerces x into vector if isn't already sequential."
  [x]
  (if (or (sequential? x) (set? x))
    x
    [x]))

(defn setify
  "Coerces x into set if isn't already."
  [x]
  (if (set? x) x #{x}))

(defn conjs
  "Like conj but always set."
  [set x]
  (conj (or set #{}) x))

(defn conjv
  "Like conj but always vec."
  [v x]
  (conj (or v []) x))

(defn conjs-any
  "Given two values, performs to-many conj."
  [v1 v2]
  (cond
    (set? v1) (conj v1 v2)
    (nil? v1) v2
    :else     (hash-set v1 v2)))

(defn assoc-nil
  "Only assoc if existing value is `nil`."
  [e attr v]
  (update e attr #(or % v)))

(defn update-some
  "Only update if value exists at the given attr."
  [e attr f & args]
  (if (contains? e attr)
    (apply update e attr f args)
    e))

(defn map-all
  "Like map but exhausts all colls."
  [f pad & colls]
  (letfn [(pick [xs]
            (if (seq xs)
              (first xs)
              pad))]
    (lazy-seq
     (when (some seq colls)
       (cons
        (apply f (map pick colls))
        (apply map-all f pad (map rest colls)))))))

(defn mapcat-lazy
  "Fully lazy version of mapcat."
  [f coll]
  (for [x  coll
        x* (f x)]
    x*))

(defn merge-deep
  "Like merge, but merges recusively. Maps are merged via merge
  semantics, and vectors are merged positionally."
  [& args]
  (letfn [(m [& args]
            (if (every? map? args)
              (apply merge-with m args)
              (if (every? sequential? args)
                (apply map-all (partial merge-with m) nil args)
                (last args))))]
    (apply m (remove nil? args))))

(defn merge-deps
  "Useful for merging a dependency graph where the list of an entry's
  dependencies needs to stay a vector."
  [& deps]
  (apply merge-with (comp vec distinct concat) deps))

(defn remove-nth
  "If you really must, removes the nth element of a vector. But consider
  using a hashed map instead."
  [coll n]
  (vec (concat (subvec coll 0 n)
               (subvec coll (inc n)))))

(s/def ::derive-pairs
  (s/* (s/cat :child (s/alt :one keyword?
                            :many (s/coll-of keyword?))
              :parent keyword?)))

(defn derive-pairs
  "Given a seq of child - parent paris, derives them into the given
  hierarchy, or into a new one if not supplied. The child in each pair
  can be cardinality many, and will be distributed against the
  corresponding parent."
  ([pairs]
   (derive-pairs (make-hierarchy) pairs))
  ([h pairs]
   (s/assert ::derive-pairs pairs)
   (letfn [(lift [[c p]]
             (for [c* (seqify c)]
               [c* p]))]
     (->> pairs
          (partition 2)
          (mapcat lift)
          (reduce (partial apply derive) h)))))

(defn- simple-merge-hierarchies
  [& h]
  (apply merge-with (partial merge-with set/union) h))

(defn- check-cyclical-derivation
  [h]
  (if (some
       (fn [[_ families]]
         (some
          (fn [[k family]]
            (contains? family k))
          families))
       h)
    (throw (ex-info "Cyclical derivation" h))
    h))

(defn- extrapolate-families
  [h]
  (letfn [(extrapolate [families]
            (reduce-kv
             (fn [m k family]
               (->> family
                    (keep families)
                    (apply set/union family)
                    (assoc m k)))
             {}
             families))]
    (some-> h
            (update :ancestors extrapolate)
            (update :descendants extrapolate))))

(def merge-hierarchies
  "Merges hierarchies, extrapolating implicit ancestors and descendants,
  and checking for cyclical derivations."
  (comp check-cyclical-derivation
        extrapolate-families
        simple-merge-hierarchies))

(defn descendants+
  [h x]
  (when x
    (conj (or (descendants h x) #{}) x)))

(defn ancestors+
  [h x]
  (when x
    (conj (or (ancestors h x) #{}) x)))

(defn split-keys
  "Given a map, and a set of key vectors, returns a seq of maps
  corresponding to a select-keys with each key vector. A map
  containing the remaining elements that were not selected is returned
  as the final element of the sequence."
  [m & ks]
  (conj (mapv #(select-keys m %) ks)
        (apply dissoc m (flatten ks))))

(defn index-comparator
  "Given a vector, returns a comparator by element indices."
  [v]
  (fn [x y]
    (let [ix (.indexOf v x)
          iy (.indexOf v y)]
      (if (or (neg? ix) (neg? iy))
        (throw
         (ex-info
          "value not in comparator vector"
          {:comparator v
           :value      (cond
                         (neg? ix) x
                         (neg? iy) y)}))
        (< ix iy)))))

#?(:clj
   (defn get-process-name
     "Returns java process name. On most systems it will be of the form
  pid@user."
     []
     (.getName (ManagementFactory/getRuntimeMXBean))))

#?(:clj
   (defn get-process-pid
     "Returns java process pid."
     []
     (some->> (get-process-name)
              (re-find #"(\d{1,6})@\w+")
              second)))
