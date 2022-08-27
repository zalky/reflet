(ns reflet.core-test
  (:require [cljs.test :refer [deftest testing is are use-fixtures]]
            [reflet.core :as f]
            [reflet.db.normalize :as norm]))

(use-fixtures :once
  (fn [tests]
    ;; Rebinds reflet.core/random-ref to return a deterministic
    ;; entity ref. Specifically, the ref value is set to be the
    ;; provided binding attribute.
    (binding [f/*random-ref* vector]
      (tests))))

(deftest with-ref-meta
  (testing "transient is default true on fresh refs"
    (is (= (f/with-ref {:system/uuid [p]}
             (norm/ref-meta p))
           {:transient true}))
    (let [props {:p [:system/uuid "a"]}]
      (is (nil? (f/with-ref {:system/uuid [p] :in props}
                  (norm/ref-meta p))))))

  (testing "Arbitrary meta data"
    (is (= (f/with-ref {:system/uuid [p]
                        :meta        {:provisional true}}
             (norm/ref-meta p))
           {:provisional true
            :transient   true}))))

(deftest with-ref-test
  (testing "nil"
    (is (nil? (f/with-ref {})))
    (is (nil? (f/with-ref {:system/uuid []})))
    (is (nil? (f/with-ref {:system/uuid []}))))

  (testing "no-op"
    (is (let [props {:self "A"}]
          (= (f/with-ref {:in props}
               props)
             props))))

  (testing "produces uuid"
    (binding [f/*random-ref* (fn [k] [k (random-uuid)])]
      (is (let [[attr uuid] (f/with-ref {:system/uuid [self]} self)]
            (and (= attr :system/uuid)
                 (uuid? uuid))))))

  (testing "local bindings"
    (is (= (f/with-ref {:system/uuid [self]}
             self)
           [:system/uuid :self]))
    (is (= (f/with-ref {:system/uuid [self target]}
             [self target])
           [[:system/uuid :self] [:system/uuid :target]]))
    (is (= (f/with-ref {:system/uuid    [self target]
                        :component/uuid [local]}
             [self target local])
           [[:system/uuid :self] [:system/uuid :target] [:component/uuid :local]]))
    (is (= (f/with-ref {:system/uuid    [self target]
                        :component/uuid [local]}
             [self target local])
           [[:system/uuid :self] [:system/uuid :target] [:component/uuid :local]]))
    (is (= (f/with-ref {:system/uuid    [search/self search/target]
                        :component/uuid [:search/local]}
             [self target local])
           [[:system/uuid :search/self]
            [:system/uuid :search/target]
            [:component/uuid :search/local]])))

  (let [props {}]
    (testing "props local re-binding"
      (is (= (f/with-ref {:system/uuid [self] :in props}
               [self props])
             [[:system/uuid :self] {:self [:system/uuid :self]}]))
      (is (= (f/with-ref {:system/uuid    [self target]
                          :component/uuid [local]
                          :in             props}
               [self target local props])
             [[:system/uuid :self]
              [:system/uuid :target]
              [:component/uuid :local]
              {:self   [:system/uuid :self]
               :target [:system/uuid :target]
               :local  [:component/uuid :local]}]))
      (is (= (f/with-ref {:system/uuid    [search/self search/target]
                          :component/uuid [:search/local]
                          :in             props}
               [self target local props])
             [[:system/uuid :search/self]
              [:system/uuid :search/target]
              [:component/uuid :search/local]
              {:search/self   [:system/uuid :search/self]
               :search/target [:system/uuid :search/target]
               :search/local  [:component/uuid :search/local]}]))))

  (testing "props external re-binding"
    (let [props {:search/self "external"}]
      (is (= (f/with-ref {:system/uuid    [search/self search/target]
                          :component/uuid [:search/local]
                          :in             props}
               [self target local props])
             ["external"
              [:system/uuid :search/target]
              [:component/uuid :search/local]
              {:search/self   "external"
               :search/target [:system/uuid :search/target]
               :search/local  [:component/uuid :search/local]}]))))

  (testing "props local fresh binding"
    ;; Note that unlike the previous two tests, props is not being
    ;; re-bound, it is bound fresh.
    (is (= (f/with-ref {:system/uuid [self]
                        :in          props}
             [self props])
           [[:system/uuid :self] {:self [:system/uuid :self]}]))))
