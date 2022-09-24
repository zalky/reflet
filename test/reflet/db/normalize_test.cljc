(ns reflet.db.normalize-test
  (:require [reflet.db.normalize :as norm]
            #?@(:clj [[clojure.test :refer [deftest testing is use-fixtures]]]
                :cljs [[cljs.test :refer [deftest testing is use-fixtures]
                        :include-macros true]])))

(deftest refer-many-test
  (let [opts {:id-attrs #{:i}}]
    (testing "join many"
      (testing "nil"
        (is (= (norm/refer-many nil opts)
               nil))
        (is (= (norm/refer-many 1 opts)
               nil))
        (is (= (norm/refer-many [] opts)
               nil))
        (is (= (norm/refer-many [1] opts)
               nil))
        (is (= (norm/refer-many [[1]] opts)
               nil))
        (is (= (norm/refer-many [:i 1] opts)
               nil))
        (is (= (norm/refer-many {:i 1} opts)
               nil)))

      (testing "valid"
        (is (= (norm/refer-many [[:i 1]] opts)
               '([:i 1])))
        (is (= (norm/refer-many [{:i 1}] opts)
               '([:i 1])))
        (is (= (norm/refer-many [[:i 1] [:i 2]] opts)
               '([:i 1] [:i 2])))
        (is (= (norm/refer-many [{:i 1} {:i 2}] opts)
               '([:i 1] [:i 2])))
        (is (= (norm/refer-many [[:i 1] {:i 2}] opts)
               '([:i 1] [:i 2])))
        (is (= (norm/refer-many [{:i 1 :a 2}
                                 {:i 2 :b 3}] opts)
               '([:i 1] [:i 2])))
        (is (= (norm/refer-many [{:i 1 :a {:i 2}}] opts)
               '([:i 1]))))

      (testing "errors"
        (is (thrown? #?(:clj Exception :cljs js/Error)
                     (norm/refer-many [1 {:i 1}] opts)))))))

(defn normalize-i
  [tx]
  (norm/normalize tx {:id-attrs #{:i}}))

(deftest test-normalize
  (testing "no normalization"
    (is (= (norm/normalize nil nil)
           [nil]))
    (is (= (norm/normalize {} nil)
           [{}]))
    (is (= (norm/normalize {:a 1} nil)
           [{:a 1}]))
    (is (= (norm/normalize {:a [{:a 1}]} nil)
           [{:a [{:a 1}]}]))
    (is (= (norm/normalize {:a [1 {:a 1}]} nil)
           [{:a [1 {:a 1}]}])))

  (testing "cardinality one"
    (is (= (normalize-i {:a {:i 1 :a 2}})
           [{:a [:i 1]} {:i 1, :a 2}]))
    (is (= (mapcat normalize-i [{:a {:i 1 :a 2}}
                                {:a {:i 2 :a 3}}])
           [{:a [:i 1]}
            {:i 1, :a 2}
            {:a [:i 2]}
            {:i 2, :a 3}]))
    (is (= (normalize-i {:a [:i 1]})
           [{:a [:i 1]}])))

  (testing "cardinality many"
    (is (= (normalize-i {:a [{:i 1 :a 2}]})
           [{:a [[:i 1]]}
            {:i 1, :a 2}]))
    (is (= (normalize-i {:a #{{:i 1 :a 2}}})
           [{:a #{[:i 1]}}
            {:i 1, :a 2}]))
    (is (= (normalize-i {:a '({:i 1 :a 2})})
           [{:a '([:i 1])}
            {:i 1, :a 2}]))
    (is (= (normalize-i {:a [{:i 1 :a 2}
                             {:i 2 :a 2}]})
           [{:a [[:i 1] [:i 2]]}
            {:i 1, :a 2}
            {:i 2, :a 2}]))
    (is (= (normalize-i {:a [[:i 1]]})
           [{:a [[:i 1]]}]))
    (is (= (normalize-i {:a [[:i 1] {:i 2 :a 1}]})
           [{:a [[:i 1] [:i 2]]}
            {:i 2 :a 1}]))
    (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo
                             :cljs js/Error)
                          #"Cannot mix entities and non-entities"
                          (normalize-i {:a [1 {:i 1}]}))))

  (testing "nested"
    (is (= (normalize-i {:a [{:i 1
                              :a {:i 2
                                  :a 3}
                              :b [{:i 3
                                   :a {:i 4
                                       :c 2}}
                                  {:i 5
                                   :c 1}]}]})
           [{:a [[:i 1]]}
            {:i 1, :a [:i 2], :b [[:i 3] [:i 5]]}
            {:i 2, :a 3}
            {:i 3, :a [:i 4]}
            {:i 4, :c 2}
            {:i 5, :c 1}])))

  (testing "multi index"
    (let [tx (norm/normalize {:a [{:i 1
                                   :n "1"
                                   :a 1}]}
                             {:id-attrs #{:i :n}})]
      (is (or (= tx [{:a [[:n "1"]]}
                     {:i 1, :n "1", :a 1}])
              (= tx [{:a [[:i 1]]}
                     {:i 1, :n "1", :a 1}])))))

  (testing "refer-fn"
    (is (= (norm/normalize {:a [{:i 1
                                 :a 1}]}
                           {:id-attrs #{:i}
                            :refer-fn #(select-keys %1 [%2])})
           [{:a [{:i 1}]}
            {:i 1, :a 1}])))

  (letfn [(add-id [e]
            (assoc e :db/id (hash e)))]
    (testing "id-fn"
      (is (= (norm/normalize {:a [{:i 1
                                   :a 1}]}
                             {:id-attrs     #{:db/id}
                              :transform-fn add-id})
             #?(:clj [{:a     [[:db/id -418653551]]
                       :db/id 1034430941}
                      {:i     1
                       :a     1
                       :db/id -418653551}]
                :cljs [{:a [[:db/id -914186598]], :db/id 163609232}
                       {:i 1, :a 1, :db/id -914186598}]))))))


