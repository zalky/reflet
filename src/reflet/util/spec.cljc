(ns reflet.util.spec
  (:require [clojure.spec.alpha :as s]))

(defn conform-to
  "Given a spec, returns a version of that spec that additionally
  applies f to the conformed value."
  [spec f]
  (s/and spec (s/conformer f)))

(defn non-conformer
  "Returns a version of the given spec that checks but does not conform
  the value. Handles errors within the `spec` transparently."
  [spec]
  (let [v       (volatile! nil)
        tap     #(vreset! v %)
        restore #(case % ::s/invalid % @v)]
    (s/and (s/conformer tap)
           spec
           (s/conformer restore))))

(defn parse
  "Given a spec and a value, conforms the value, or throws an error if
  the value does not conform to the spec. Note the difference with
  clojure.spec.alpha/assert, which does not conform the return value."
  [spec value]
  (let [form (s/conform spec value)]
    (if (= form ::s/invalid)
      (-> "Parse error"
          (ex-info (s/explain-data spec value))
          (throw))
      form)))

(defn cardinality-many?
  [x]
  (or (sequential? x) (set? x)))

(defn cardinality-many-conformed
  "Given a spec, returns a version that conforms to cardinality many
  arguments."
  [spec]
  (s/coll-of spec :kind cardinality-many?))

(defn any-cardinality-conformed
  "Given a spec, returns a version of that spec that conforms to any
  cardinality. Just like an or spec, the returned conformed value will
  be a [type form] pair."
  [spec]
  (s/or :one  spec
        :many (cardinality-many-conformed spec)))

(defn any-cardinality
  "Given a spec, returns a version of that spec that conforms to any
  cardinality. Returns the conformed value from the spec
  unchanged. Optional kwarg `:coerce-many` coerces values to
  cardinality many."
  [spec & {:keys [coerce-many]}]
  (conform-to
    (any-cardinality-conformed spec)
    (fn [[cardinality form]]
      (case cardinality
        :one  (cond-> form coerce-many list)
        :many form))))

(defn spec?
  "Returns true if `spec` is a valid spec. Note that this is not the
  same thing as `clojure.spec.alpha/spec?`, which only returns true if
  `spec` is a valid spec object."
  [spec]
  (try
    (do (s/conform spec nil) true)
    (catch #?(:clj Exception
              :cljs js/Error) e
      false)))

;;;; Specs

(s/def ::ref
  (s/tuple qualified-keyword? some?))
