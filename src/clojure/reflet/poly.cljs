(ns reflet.poly
  "Provides polymorphic dispatch resolution."
  (:require [cinch.core :as util]
            [clojure.spec.alpha :as s]))

(defn- ancestors+
  [h x]
  (cons x (ancestors h x)))

(defn- prefers?
  [prefers x y]
  (boolean (contains? (get prefers x) y)))

(defn- dominates?
  [h prefers x y]
  (or (prefers? prefers x y) (isa? h x y)))

(defn- throw-resolve-error!
  [{dispatch :dispatch-val} x y]
  (throw
   (js/Error.
    (str "Multiple dispatch candidates for: " dispatch
         " -> " x " and " y
         ", and neither is preferred"))))

(defn- poly-resolve-rf
  [{h        :hierarchy
    p        :prefers
    dispatch :dispatch-val
    :as      context}]
  (let [dom? (partial dominates? h p)]
    (fn [r k]
      (if (isa? h dispatch k)
        (cond
          (nil? r)   k
          (dom? k r) k
          (dom? r k) r
          :else      (throw-resolve-error! context r k))
        r))))

(defn poly-resolve
  "Given a :hierarchy, :prefers table, a set of dispatch :candidates,
  and an actual :dispatch value, returns a matched candidate by
  polymorphic comparison. Works for both vector and primitive dispatch
  values."
  [{candidates :candidates
    h          :hierarchy
    dispatch   :dispatch-val
    :as        context}]
  (if h
    (-> context
        (poly-resolve-rf)
        (reduce nil candidates))
    dispatch))

(defn- throw-prefer-error!
  [y x]
  (throw
   (js/Error.
    (str "Prefer conflict: " y " is already preferred to " x))))

(defn- prefer-rf
  [prefers [x y]]
  (when (prefers? prefers y x)
    (throw-prefer-error! y x))
  (update prefers x util/conjs y))

(defn prefer-pairs
  "Given a sequence of preference pairs, adds them to a `prefers`
  table. Each element in a pair can be cardinality one or
  many. Cardinality many elements are distributed combinatorially."
  ([xs]
   (prefer-pairs {} xs))
  ([prefers xs]
   (s/assert ::util/derive-pairs xs)
   (->> xs
        (partition 2)
        (reduce prefer-rf prefers))))
