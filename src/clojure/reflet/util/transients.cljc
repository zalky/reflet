(ns reflet.util.transients)

(defn into!
  "Transient version of clojure.core/into"
  [to from]
  (reduce conj! to from))

(defn update!
  "Transient version of clojure.core/update"
  ([m k f]
   (assoc! m k (f (get m k))))
  ([m k f x]
   (assoc! m k (f (get m k) x)))
  ([m k f x y]
   (assoc! m k (f (get m k) x y)))
  ([m k f x y z]
   (assoc! m k (f (get m k) x y z)))
  ([m k f x y z & more]
   (assoc! m k (apply f (get m k) x y z more))))
