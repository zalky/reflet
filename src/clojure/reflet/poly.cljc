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
   (ex-info
    (str "Multiple dispatch entries for: " dispatch
         " -> " x " and " y
         ", and neither is preferred")
    {})))

(defn- resolve-rf
  [{h   :hierarchy
    p   :prefers
    val :dispatch-val
    :as context}]
  (let [dom? (partial dominates? h p)]
    (fn [[r :as er] [k :as ek]]
      (if (isa? h val k)
        (cond
          (nil? er)  ek
          (dom? k r) ek
          (dom? r k) er
          :else      (throw-resolve-error! context r k))
        er))))

(defn- poly-resolve*
  [{entries :entries
    h       :hierarchy
    p       :prefers
    val     :dispatch-val
    :as     context}]
  (if (or h p)
    (-> (resolve-rf context)
        (reduce nil entries)
        (second))
    (when (contains? entries val)
      (get entries val))))

(defn poly-resolve
  "Given a map of entries, performs a polymorphic lookup, possibly
  returning a default value. Options include:

  :entries
            A map of entries against which the lookup will be
            performed. Keys should be dispatch values.

  :hierarchy
            A Clojure hierarchy representing isa relationships
            between types.

  :prefers
            A prefers map. The keys dominate the values, which
            are provided as sets.

  :dispatch-val
            The dispatch value which will be matched against the
            entries.

  :default-dispatch-val
            If no match is found, this will be used to lookup
            the default entry."
  [{entries     :entries
    val         :dispatch-val
    default-val :default-dispatch-val
    :or         {default-val :default}
    :as         context}]
  (or (poly-resolve* context)
      (get entries default-val)
      (-> "Could not resolve entry"
          (ex-info {:dispatch-val val})
          (throw))))

(defn- throw-prefer-error!
  [y x]
  (throw
   (ex-info
    (str "Prefer conflict: " y " is already preferred to " x)
    {})))

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
