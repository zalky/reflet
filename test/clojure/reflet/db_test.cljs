(ns reflet.db-test
  (:require [cljs.test :as t :refer [deftest testing is]]
            [reflet.fixtures :as fix]
            [reflet.db :as db]))

(t/use-fixtures :each fix/base-fixtures)

(defn db
  ([]
   (merge (db/new-db)
          {::db/data  {::db/tick 0}
           ::db/index {}}))
  ([data & {:keys [tick]
            :or   {tick (-> (db) ::db/data ::db/tick inc)}}]
   (-> (db)
       (assoc ::db/data data)
       (assoc-in [::db/data ::db/tick] tick)
       (assoc-in [::db/index ::db/tick] tick))))

(deftest add-test
  (testing "add normalized entities"
    (testing "nil"
      (is (= (db/mergen (db) nil)
             (db {})))
      (is (= (db/mergen (db) [])
             (db {})))
      (is (= (db/mergen (db) [:system/uuid 1])
             (db {})))
      (is (= (db/mergen (db) [[:system/uuid 1] [:system/uuid 2]])
             (db {}))))

    (testing "with no attributes"
      (is (= (db/mergen (db) {:system/uuid 1})
             (db {[:system/uuid 1] {:system/uuid 1}})))
      (is (= (db/mergen (db) [{:system/uuid 1}])
             (db {[:system/uuid 1] {:system/uuid 1}}))))

    (testing "with attributes"
      (testing "non-colls"
        (is (= (db/mergen (db) [{:system/uuid 1 :a "a"}])
               (db {[:system/uuid 1] {:system/uuid 1 :a "a"}}))))
      (testing "non-entity maps"
        (is (= (db/mergen (db) [{:system/uuid 1 :a {:b "2"}}])
               (db {[:system/uuid 1] {:system/uuid 1, :a {:b "2"}}}))))
      (testing "colls"
        (is (= (db/mergen (db) [{:system/uuid 1 :a [1 2]}])
               (db {[:system/uuid 1] {:system/uuid 1, :a [1 2]}})))
        (is (= (db/mergen (db) [{:system/uuid 1 :a #{1 2}}])
               (db {[:system/uuid 1] {:system/uuid 1, :a #{1 2}}})))
        (is (= (db/mergen (db) [{:system/uuid 1 :a '(1 2)}])
               (db {[:system/uuid 1] {:system/uuid 1, :a '(1 2)}}))))
      (testing "vectors"
        (is (= (db/mergen (db) [{:system/uuid 1 :a {:b "2"}}])
               (db {[:system/uuid 1] {:system/uuid 1, :a {:b "2"}}})))))
    
    (testing "with join"
      (is (= (db/mergen (db) {:system/uuid 1 :a {:system/uuid 2}})
             (db {[:system/uuid 1] {:system/uuid 1, :a [:system/uuid 2]},
                  [:system/uuid 2] {:system/uuid 2}}))))
    
    (testing "with many join"
      (testing "vectors"
        (is (= (db/mergen (db) [{:system/uuid 1
                                 :a [{:system/uuid 2} {:system/uuid 3}]}])
               (db {[:system/uuid 1]
                    {:system/uuid 1, :a [[:system/uuid 2] [:system/uuid 3]]},
                    [:system/uuid 2] {:system/uuid 2},
                    [:system/uuid 3] {:system/uuid 3}}))))
      (testing "lists"
        (is (= (db/mergen (db) [{:system/uuid 1
                                 :a '({:system/uuid 2} {:system/uuid 3})}])
               (db {[:system/uuid 1]
                    {:system/uuid 1, :a '([:system/uuid 2] [:system/uuid 3])},
                    [:system/uuid 2] {:system/uuid 2},
                    [:system/uuid 3] {:system/uuid 3}}))))
      (testing "sets"
        (is (= (db/mergen (db) [{:system/uuid 1
                                 :a #{{:system/uuid 2} {:system/uuid 3}}}])
               (db {[:system/uuid 1]
                    {:system/uuid 1, :a #{[:system/uuid 2] [:system/uuid 3]}},
                    [:system/uuid 3] {:system/uuid 3},
                    [:system/uuid 2] {:system/uuid 2}})))))
    
    (testing "with mutual joins"
      (is (= (db/mergen (db) [{:system/uuid 1
                               :a {:system/uuid 2
                                   :a "a"}}
                              {:system/uuid 2
                               :b {:system/uuid 1
                                   :b "b"}}])
             (db {[:system/uuid 1] {:system/uuid 1
                                    :a [:system/uuid 2]
                                    :b "b"}
                  [:system/uuid 2] {:system/uuid 2
                                    :a "a"
                                    :b [:system/uuid 1]}}))))
    
    (testing "with recursive joins"
      (is (= (db/mergen (db) [{:system/uuid 1
                               :a [{:system/uuid 2}
                                   {:system/uuid 3
                                    :a [{:system/uuid 1 :b "b"}]}]}])
             (db {[:system/uuid 1] {:system/uuid 1
                                    :a [[:system/uuid 2] [:system/uuid 3]]
                                    :b "b"}
                  [:system/uuid 2] {:system/uuid 2}
                  [:system/uuid 3] {:system/uuid 3
                                    :a [[:system/uuid 1]]}}))))))


(deftest remove-test
  (testing "remove normalized entities"
    (testing "nil"
      (is (= (-> (db {[:system/uuid 1] {:system/uuid 1, :a "a"}} :tick 0)
                 (db/dissocn nil))
             (db {[:system/uuid 1] {:system/uuid 1, :a "a"}})))
      (is (= (-> (db {[:system/uuid 1] {:system/uuid 1, :a "a"}} :tick 0)
                 (db/dissocn [])))))

    (testing "valid"
      (is (= (-> (db {[:system/uuid 1] {:system/uuid 1, :a "a"}} :tick 0)
                 (db/dissocn [:system/uuid 1]))
             (db {})))
      (is (= (-> (db {[:system/uuid 1] {:system/uuid 1, :a "a"}} :tick 0)
                 (db/dissocn {:system/uuid 1}))
             (db {})))
      (is (= (-> (db {[:system/uuid 1] {:system/uuid 1, :a "a"}
                      [:system/uuid 2] {:system/uuid 2, :a "a"}} :tick 0)
                 (db/dissocn [:system/uuid 1] [:system/uuid 2]))
             (db {})))
      (is (= (-> (db {[:system/uuid 1] {:system/uuid 1, :a [:system/uuid 2]}
                      [:system/uuid 2] {:system/uuid 2, :b "b"}} :tick 0)
                 (db/dissocn {:system/uuid 1 :a {:system/uuid 2 :b "b"}}))
             (db {[:system/uuid 2] {:system/uuid 2, :b "b"}})))      
      (is (= (-> (db {[:system/uuid 1] {:system/uuid 1, :a "a"}} :tick 0)
                 (db/dissocn {:system/uuid 1, :b "b"}))
             (db {})))
      (is (= (-> (db {::link-1 [:system/uuid 1]
                      ::link-2 [[:system/uuid 1] [:system/uuid 2]]
                      [:system/uuid 1] {:system/uuid 1, :a [:system/uuid 2]}
                      [:system/uuid 2] {:system/uuid 2, :b "b"}} :tick 0)
                 (db/dissocn ::link-1 ::link-2))
             (db {[:system/uuid 1] {:system/uuid 1, :a [:system/uuid 2]}
                  [:system/uuid 2] {:system/uuid 2, :b "b"}}))))))

(deftest update-test
  (testing "update normalized entity"    
    (testing "valid"
      (is (= (-> (db {[:system/uuid 1] {:system/uuid 1 :a 1}} :tick 0)
                 (db/update-inn [[:system/uuid 1] :a] inc))
             (db {[:system/uuid 1] {:system/uuid 1, :a 2}})))
      (is (= (-> (db {[:system/uuid 1] {:system/uuid 1}} :tick 0)
                 (db/update-inn [[:system/uuid 1] :a] inc))
             (db {[:system/uuid 1] {:system/uuid 1, :a 1}}))))

    (testing "errors"
      (is (thrown? js/Error (-> (db {[:system/uuid 1] {:system/uuid 1 :a 1}})
                                (db/update-inn [nil :a] inc)))))))

(deftest link-test
  (testing "add normalized entity link"
    (testing "errors"
      (testing "invalid attr"
        (is (thrown? js/Error (db/assocn (db) nil nil)))
        ;; TODO The next ones are valid with current pre-check.
        ;; Do we want some more restrictive check than identity?
        ;(is (thrown? js/Error (db/assocn (db) 1 nil)))
        ;(is (thrown? js/Error (db/assocn (db) "attr" nil)))
        ))
    
    (testing "valid"
      (is (= (db/assocn (db) :attr [:system/uuid 1])
             (db {:attr [:system/uuid 1]})))
      (is (= (db/assocn (db) :attr [[:system/uuid 1]])
             (db {:attr [[:system/uuid 1]]})))
      (is (= (db/assocn (db) :attr {:system/uuid 1})
             (db {[:system/uuid 1] {:system/uuid 1}
                  :attr            [:system/uuid 1]})))      
      (is (= (db/assocn (db) :attr [{:system/uuid 1}])
             (db {[:system/uuid 1] {:system/uuid 1}
                  :attr            [[:system/uuid 1]]})))
      (is (= (db/assocn (db) :attr [{:system/uuid 1} {:system/uuid 2}])
             (db {[:system/uuid 2] {:system/uuid 2}
                  [:system/uuid 1] {:system/uuid 1}
                  :attr            [[:system/uuid 1] [:system/uuid 2]]}))))))

(deftest pull-test
  (let [dbv (db {[:system/uuid "name"]     {:system/uuid    "name"
                                            :kr/name        "name"
                                            :kr/description "description"
                                            :kr/null        nil
                                            :kr/join        [:system/uuid "join"]}
                 [:system/uuid "join"]     {:system/uuid "join"
                                            :kr/name     "join"
                                            :kr/label    "label"
                                            :kr/join     [[:system/uuid "nested 1"]
                                                          [:system/uuid "nested 2"]]}
                 [:system/uuid "nested 1"] {:system/uuid "nested 1"
                                            :kr/name     "nested 1"}
                 [:system/uuid "nested 2"] {:system/uuid "nested 2"
                                            :kr/name     "nested 2"
                                            :kr/join     [:system/uuid "nested 3"]}
                 [:system/uuid "nested 3"] {:system/uuid    "nested 3"
                                            :kr/name        "nested 3"
                                            :kr/description "leaf"}
                 ::link                    [[:system/uuid "nested 1"]
                                            [:system/uuid "nested 2"]]})]

    (testing "pull"
      (testing "attr"
        (is (= (db/pull dbv
                        :kr/name
                        [:system/uuid "name"])
               "name"))

        (is (= (db/pull dbv
                        {:kr/join [:system/uuid
                                   :kr/name
                                   :kr/label]}
                        [:system/uuid "name"])
               {:system/uuid "join"
                :kr/name     "join"
                :kr/label    "label"})))

      (testing "pattern"
        (is (= (db/pull dbv
                        [:system/uuid
                         :kr/name
                         :kr/description
                         :kr/null]
                        [:system/uuid "name"])
               {:system/uuid    "name"
                :kr/name        "name"
                :kr/description "description"
                :kr/null        nil})))

      (testing "join one"
        (is (= (db/pull dbv
                        [:system/uuid
                         :kr/name
                         :kr/description
                         {:kr/join [:system/uuid
                                    :kr/name
                                    :kr/label]}]
                        [:system/uuid "name"])
               {:system/uuid    "name"
                :kr/name        "name"
                :kr/description "description"
                :kr/join        {:system/uuid "join"
                                 :kr/name     "join"
                                 :kr/label    "label"}})))

      (testing "join many"
        (is (= (db/pull dbv
                        [:system/uuid
                         :kr/name
                         :kr/label
                         {:kr/join [:system/uuid
                                    :kr/name
                                    :kr/label]}]
                        [:system/uuid "join"])
               {:system/uuid "join"
                :kr/name     "join"
                :kr/label    "label"
                :kr/join     [{:system/uuid "nested 1"
                               :kr/name     "nested 1"}
                              {:system/uuid "nested 2"
                               :kr/name     "nested 2"}]}))

        (is (= (db/pull dbv
                        [:system/uuid
                         {:kr/name ['*]}]
                        [:system/uuid "name"])
               {:system/uuid "name"
                :kr/name     "name"}))))

    (testing "Wildcard query"
      (is (= (db/pull dbv
                      '[*
                        {:kr/join [*]}]
                      [:system/uuid "name"])
             {:system/uuid    "name"
              :kr/name        "name"
              :kr/description "description"
              :kr/null        nil
              :kr/join
              {:system/uuid "join"
               :kr/name     "join"
               :kr/label    "label"
               :kr/join     [[:system/uuid "nested 1"]
                             [:system/uuid "nested 2"]]}})))

    (testing "Link query"
      (is (= (db/pull dbv {::link [:system/uuid :kr/name]})
             [{:system/uuid "nested 1"
               :kr/name     "nested 1"}
              {:system/uuid "nested 2"
               :kr/name     "nested 2"}])))

    (testing "Infinite recursive pull"
      (is (= (db/pull dbv
                      [:kr/name
                       :kr/description
                       {:kr/join '...}]
                      [:system/uuid "name"])
             {:kr/name        "name"
              :kr/description "description"
              :kr/join        {:kr/name "join"
                               :kr/join [{:kr/name "nested 1"}
                                         {:kr/name "nested 2"
                                          :kr/join {:kr/name        "nested 3"
                                                    :kr/description "leaf"}}]}})))

    (testing "Limited recursive pull"
      (is (= (db/pull dbv
                      [:kr/name
                       :kr/description
                       {:kr/join 2}]
                      [:system/uuid "name"])
             {:kr/name        "name"
              :kr/description "description"
              :kr/join        {:kr/name "join"
                               :kr/join [{:kr/name "nested 1"}
                                         {:kr/name "nested 2"
                                          :kr/join [:system/uuid "nested 3"]}]}})))

    (testing "Infinitely recursive wildcard query (pull everything)"
      (is (= (db/pull dbv
                      '[*
                        {:kr/join ...}]
                      [:system/uuid "name"])
             {:system/uuid    "name"
              :kr/name        "name"
              :kr/description "description"
              :kr/null        nil
              :kr/join        {:system/uuid "join"
                               :kr/name     "join"
                               :kr/label    "label"
                               :kr/join     [{:system/uuid "nested 1"
                                              :kr/name     "nested 1"}
                                             {:system/uuid "nested 2"
                                              :kr/name     "nested 2"
                                              :kr/join     {:system/uuid    "nested 3"
                                                            :kr/name        "nested 3"
                                                            :kr/description "leaf"}}]}})))))

(defn- provisional?
  [ref]
  (boolean (some-> ref db/ref-meta :provisional)))

(defn- pull-provisional
  [db e-ref]
  (let [pfn (db/get-pull-fn)]
    (pfn {:id-attrs (::db/id-attrs db)
          :db       (::db/data db)
          :ref      e-ref
          :join?    provisional?}
         '[{* ...}])))

(deftest pull-predicate-test
  (let [r1 (db/random-ref :system/uuid {:provisional true})
        r2 (db/random-ref :system/uuid {:provisional true})
        r3 (db/random-ref :system/uuid {:provisional true})
        r4 (db/random-ref :system/uuid)
        r5 (db/random-ref :system/uuid)]
    (let [dbv (db {r1 {:system/uuid (second r1)
                       :kr/join     r2
                       :kr/join2    r4}
                   r2 {:system/uuid (second r2)
                       :kr/join     [r3 r4]}
                   r3 {:system/uuid (second r3)
                       :kr/join     r5}
                   r4 {:system/uuid (second r4)}
                   r5 {:system/uuid    (second r5)
                       :kr/name        "r5"
                       :kr/description "leaf"}})]

      (is (= (pull-provisional dbv r1)
             {:system/uuid (second r1)
              :kr/join     {:system/uuid (second r2)
                            :kr/join     [{:system/uuid (second r3)
                                           :kr/join     r5}
                                          r4]}
              :kr/join2    r4})))))
