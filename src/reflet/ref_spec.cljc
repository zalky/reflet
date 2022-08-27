(ns reflet.ref-spec
  (:require [reflet.util.spec :as s*]
            [clojure.spec.alpha :as s]))

(s/def ::binding
  (s*/conform-to
    (s/or :keyword keyword?
          :symbol symbol?)
    (fn [[_ binding*]]
      {:key (keyword binding*)
       :sym (symbol (name binding*))})))

(s/def ::binding-vector
  (s/coll-of ::binding :kind vector?))

(s/def ::binding-map
  (s*/conform-to
    (s/nilable (s/map-of qualified-keyword? ::binding-vector))
    (fn [binding-map]
      (mapcat
       (fn [[k binding-vector]]
         (map
          (fn [parsed]
            (assoc parsed :attr k))
          binding-vector))
       binding-map))))
