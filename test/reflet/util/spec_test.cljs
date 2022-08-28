(ns reflet.util.spec-test
  (:require [reflet.util.spec :as s*]
            [clojure.spec.alpha :as s]
            [clojure.test :refer [deftest testing is]]))

(def A (random-uuid))
(def B (random-uuid))
(def C (random-uuid))

(deftest test-ref-conform
  (testing "ref conform basic"
    (is (= (s/conform ::s*/ref-any nil)
           ::s/invalid))
    (is (= (s/conform ::s*/ref-any [])
           '()))
    (is (= (s/conform ::s*/ref-any [nil])
           ::s/invalid))
    (is (= (s/conform ::s*/ref-any :bad-ref)
           ::s/invalid))
    (is (= (s/conform ::s*/ref-any [:bad-ident A])
           ::s/invalid))
    (is (= (s/conform ::s*/ref-any [:system/uuid A])
           [:system/uuid A])))

  (testing "ref conform from"
    (testing "cardinality one"
      (testing "uuid"
        (is (= (s/conform ::s*/ref-any A)
               ::s/invalid)))

      (testing "entity"
        (is (= (s/conform ::s*/ref-any {:system/uuid A
                                        :kr/type :entity})
               ::s/invalid))))

    (testing "cardinality many"
      (testing "ref"
        (is (= (s/conform ::s*/ref-any [[:system/uuid A] [:system/uuid B]])
               [[:system/uuid A] [:system/uuid B]])))

      (testing "uuid"
        (is (= (s/conform ::s*/ref-any [A B])
               ::s/invalid)))

      (testing "entity"
        (is (= (s/conform ::s*/ref-any [{:system/uuid A
                                         :kr/type :entity}
                                        {:system/uuid B
                                         :kr/type :entity}])
               ::s/invalid)))

      (testing "with heterogenous"
        (is (= (s/conform ::s*/ref-any [A
                                        [:system/uuid B]
                                        {:system/uuid C
                                         :kr/type :entity}])
               ::s/invalid)))

      (testing "with errors"
        (is (= (s/conform ::s*/ref-any [:bad-ref
                                        [:system/uuid B]
                                        {:system/uuid C
                                         :kr/type :entity}])
               ::s/invalid)))))

  (testing "ref coerce many"
    (is (= (s/conform ::s*/ref-coerce-many nil)
           ::s/invalid))
    (is (= (s/conform ::s*/ref-coerce-many [])
           '()))
    (is (= (s/conform ::s*/ref-coerce-many [nil])
           ::s/invalid))
    (is (= (s/conform ::s*/ref-coerce-many :bad-ref)
           ::s/invalid))
    (is (= (s/conform ::s*/ref-coerce-many [:bad-ident A])
           ::s/invalid))
    (is (= (s/conform ::s*/ref-coerce-many A)
           ::s/invalid))
    (is (= (s/conform ::s*/ref-coerce-many {:system/uuid A})
           ::s/invalid))
    (is (= (s/conform ::s*/ref-coerce-many [:system/uuid A])
           [[:system/uuid A]]))))
