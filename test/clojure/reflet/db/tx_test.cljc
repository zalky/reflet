(ns reflet.db.tx-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.walk :as walk]
            [reflet.db.tx :as tx]))

(defn compound
  [g]
  (->> [(gen/vector g) g]
       (gen/one-of)
       (gen/map gen/keyword)))

(def ref-gen
  (gen/tuple gen/keyword gen/string-ascii))

(def scalars
  (gen/one-of [gen/small-integer
               gen/string-ascii
               gen/keyword
               ref-gen
               gen/boolean]))

(def tx-gen
  (gen/recursive-gen compound scalars))

(defspec walk-tx-identity-gen 100
  ;; More than 100 is pretty slow.
  (prop/for-all [tx tx-gen]
    (= (tx/walk-tx identity tx)
       tx)))

(defn walk-maps-test
  [f tx]
  (walk/postwalk
   (fn [x]
     (cond-> x
       (map? x) f))
   tx))

(defspec walk-maps-gen 100
  ;; More than 100 is pretty slow.
  (letfn [(xform [x]
            (assoc x ::test ::test))]
   (prop/for-all [tx tx-gen]
     (= (tx/walk-maps xform tx)
        (walk-maps-test xform tx)))))

(deftest walk-maps-preserve-meta-test
  (let [tx {:id   1
            :join [(with-meta
                     {:id 2}
                     {::meta true})]}]
    (is (= (tx/walk-maps identity tx)
           tx))
    (is (= (tx/walk-maps #(assoc % :test true) tx)
           {:id   1
            :test true
            :join [(with-meta
                     {:id 2
                      :test true}
                     {::meta true})]}))))

(deftest reduce-maps-test
  ;; Careful with the traversal order: it is depth first, but the
  ;; traversal order with respect to multiple joins in a map is
  ;; undefined.
  (let [tx {:id   1
            :join [{:id   2
                    :join {:id 3}}
                   {:id   4
                    :join {:id 5}}]}
        f  #(conj %1 (:id %2))]
    (is (= (tx/reduce-maps f [] nil)
           []))
    (is (= (tx/reduce-maps f [] tx)
           [3 2 5 4 1]))
    (is (= (tx/reduce-maps f [] tx)
           (tx/reduce-maps f [] [tx])))
    (is (= (tx/reduce-maps f [] (conj [tx] {:id 6}))
           [3 2 5 4 1 6]))))
