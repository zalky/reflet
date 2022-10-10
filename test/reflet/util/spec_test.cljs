(ns reflet.util.spec-test
  (:require [reflet.util.spec :as s*]
            [clojure.spec.alpha :as s]
            [clojure.test :refer [deftest testing is]]))

(def A (random-uuid))
(def B (random-uuid))
(def C (random-uuid))

(s/def ::ref-any
  (s*/any-cardinality ::s*/ref))

(s/def ::ref-coerce-many
  (s*/any-cardinality ::s*/ref :coerce-many true))

(deftest ref-conform-test
  (testing "ref conform basic"
    (is (= (s/conform ::ref-any nil)
           ::s/invalid))
    (is (= (s/conform ::ref-any [])
           '()))
    (is (= (s/conform ::ref-any [nil])
           ::s/invalid))
    (is (= (s/conform ::ref-any :bad-ref)
           ::s/invalid))
    (is (= (s/conform ::ref-any [:bad-ident A])
           ::s/invalid))
    (is (= (s/conform ::ref-any [:system/uuid A])
           [:system/uuid A])))

  (testing "ref conform from"
    (testing "cardinality one"
      (testing "uuid"
        (is (= (s/conform ::ref-any A)
               ::s/invalid)))

      (testing "entity"
        (is (= (s/conform ::ref-any {:system/uuid A
                                     :kr/type     :entity})
               ::s/invalid))))

    (testing "cardinality many"
      (testing "ref"
        (is (= (s/conform ::ref-any [[:system/uuid A] [:system/uuid B]])
               [[:system/uuid A] [:system/uuid B]])))

      (testing "uuid"
        (is (= (s/conform ::ref-any [A B])
               ::s/invalid)))

      (testing "entity"
        (is (= (s/conform ::ref-any [{:system/uuid A
                                      :kr/type     :entity}
                                     {:system/uuid B
                                      :kr/type     :entity}])
               ::s/invalid)))

      (testing "with heterogenous"
        (is (= (s/conform ::ref-any [A
                                        [:system/uuid B]
                                     {:system/uuid C
                                      :kr/type     :entity}])
               ::s/invalid)))

      (testing "with errors"
        (is (= (s/conform ::ref-any [:bad-ref
                                        [:system/uuid B]
                                     {:system/uuid C
                                      :kr/type     :entity}])
               ::s/invalid)))))

  (testing "ref coerce many"
    (is (= (s/conform ::ref-coerce-many nil)
           ::s/invalid))
    (is (= (s/conform ::ref-coerce-many [])
           '()))
    (is (= (s/conform ::ref-coerce-many [nil])
           ::s/invalid))
    (is (= (s/conform ::ref-coerce-many :bad-ref)
           ::s/invalid))
    (is (= (s/conform ::ref-coerce-many [:bad-ident A])
           ::s/invalid))
    (is (= (s/conform ::ref-coerce-many A)
           ::s/invalid))
    (is (= (s/conform ::ref-coerce-many {:system/uuid A})
           ::s/invalid))
    (is (= (s/conform ::ref-coerce-many [:system/uuid A])
           [[:system/uuid A]]))))
