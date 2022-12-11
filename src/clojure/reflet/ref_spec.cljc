(ns reflet.ref-spec
  (:require [cinch.spec :as s*]
            [clojure.spec.alpha :as s]))

(s/def ::binding
  (s*/conform-to
    (s/or :keyword keyword?
          :symbol symbol?)
    (fn [[_ binding]]
      {:key (keyword binding)
       :sym (symbol (name binding))})))

(s/def ::binding-vector
  (s/coll-of ::binding :kind vector?))

(s/def ::binding-map
  (s*/conform-to
    (s/map-of symbol? keyword?)
    (fn [m]
      (map (fn [[sym k]]
             {:key k
              :sym sym})
           m))))

(s/def ::binding-coll
  (s/or :vector ::binding-vector
        :map    ::binding-map))

(s/def ::bindings
  (s*/conform-to
    (s/nilable (s/map-of qualified-keyword? ::binding-coll))
    (fn [bindings]
      (mapcat
       (fn [[k [_ binding-coll]]]
         (map
          (fn [parsed]
            (assoc parsed :id-attr k))
          binding-coll))
       bindings))))
