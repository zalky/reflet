(ns reflet.core-test
  (:require [cljs.test :refer [deftest testing is are use-fixtures]]
            [re-frame.registrar :as reg]
            [reflet.core :as f]
            [reflet.db :as db]
            [reflet.db.normalize :as norm]
            [reflet.fixtures :as fix]))

(defn random-ref-test
  [id-attr meta k]
  [id-attr (with-meta k meta)])

(use-fixtures :once
  (fn [tests]
    ;; Rebinds reflet.core/random-ref to return a deterministic
    ;; entity ref. Specifically, the ref value is set to be the
    ;; provided binding attribute.
    (binding [db/random-ref random-ref-test]
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
    (binding [db/random-ref (fn [k] [k (random-uuid)])]
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
                          :component/uuid [:search/local ::state]
                          :in             props}
               [self target local state props])
             [[:system/uuid :search/self]
              [:system/uuid :search/target]
              [:component/uuid :search/local]
              [:component/uuid ::state]
              {:search/self   [:system/uuid :search/self]
               :search/target [:system/uuid :search/target]
               :search/local  [:component/uuid :search/local]
               ::state        [:component/uuid ::state]}]))))

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

(deftest random-ref-cofx-test
  (let [f (reg/get-handler :cofx :random-ref)]
    (is (= (f {:db {}} {:system/uuid    [:search/self :search/target]
                        :component/uuid [:search/local]})
           {:db         {}
            :random-ref {:search/self   [:system/uuid :search/self]
                         :search/target [:system/uuid :search/target]
                         :search/local  [:component/uuid :search/local]}}))))

(deftest reg-pull-test
  (fix/run-test-sync
   ;; Note: outside a reactive context, reactions are re-run every
   ;; time they are dereferenced. Therefore some of the reactive
   ;; properties of pull queries are only really testable in a mounted
   ;; application.

   (f/reg-event-db ::init
     (fn [db [_ id join n1 n2 n3]]
       (-> db
           (db/mergen {:system/uuid    (second id)
                       :kr/name        "name"
                       :kr/description "description"
                       :kr/join        {:system/uuid (second join)
                                        :kr/name     "join"
                                        :kr/label    "label"
                                        :kr/join     [{:system/uuid (second n1)
                                                       :kr/name     "nested 1"}
                                                      {:system/uuid (second n2)
                                                       :kr/name     "nested 2"
                                                       :kr/join     {:system/uuid    (second n3)
                                                                     :kr/name        "nested 3"
                                                                     :kr/description "leaf"}}]}})
           (db/assocn ::link id))))

   (f/reg-event-db ::update-name
     (fn [db [_ id]]
       (db/update-inn db [id :kr/name] str " updated")))

   (f/with-ref {:system/uuid [id join n1 n2 n3]}
     (f/dispatch [::init id join n1 n2 n3])

     (testing "Simple join"
       (f/reg-pull ::join
         (fn [id]
           [[:system/uuid
             :kr/name
             :kr/description
             {:kr/join [:system/uuid
                        :kr/name
                        :kr/label]}]
            id]))

       (let [r (f/subscribe [::join id])]
         (is (= @r
                {:system/uuid    (second id)
                 :kr/name        "name"
                 :kr/description "description"
                 :kr/join        {:system/uuid (second join)
                                  :kr/name     "join"
                                  :kr/label    "label"}}))))

     (testing "Attribute pull"
       (f/reg-pull ::attr
         (fn [id]
           [:kr/name id]))

       (let [r (f/subscribe [::attr id])]
         (is (= @r "name"))))

     (testing "Wildcard query"
       (f/reg-pull ::wildcard
         (fn [id]
           ['[*
              {:kr/join [*]}]
            id]))

       (let [r (f/subscribe [::wildcard id join])]
         (is (= @r
                {:system/uuid    (second id)
                 :kr/name        "name"
                 :kr/description "description"
                 :kr/join        {:system/uuid (second join)
                                  :kr/name     "join"
                                  :kr/label    "label"
                                  :kr/join     [n1 n2]}}))))

     (testing "Link query"
       (f/reg-pull ::link-q
         (fn []
           [{::link [:system/uuid :kr/name]}]))

       (let [r (f/subscribe [::link-q])]
         (is (= @r
                {:system/uuid (second id)
                 :kr/name     "name"}))))

     (testing "Infinite recursive pull"
       (f/reg-pull ::recursive...
         (fn [id]
           [[:kr/name
             :kr/description
             {:kr/join '...}]
            id]))

       (let [r (f/subscribe [::recursive... id])]
         (is (= @r
                {:kr/name        "name"
                 :kr/description "description"
                 :kr/join        {:kr/name "join"
                                  :kr/join [{:kr/name "nested 1"}
                                            {:kr/name "nested 2"
                                             :kr/join {:kr/name        "nested 3"
                                                       :kr/description "leaf"}}]}}))))

     (testing "Limited recursive pull"
       (f/reg-pull ::recursive-2
         (fn [id]
           [[:kr/name
             :kr/description
             {:kr/join 2}]
            id]))

       (let [r (f/subscribe [::recursive-2 id])]
         (is (= @r
                {:kr/name        "name"
                 :kr/description "description"
                 :kr/join        {:kr/name "join"
                                  :kr/join [{:kr/name "nested 1"}
                                            {:kr/name "nested 2"
                                             :kr/join n3}]}}))))

     (testing "Infinitely recursive wildcard query (pull everything)"
       (f/reg-pull ::recursive-*
         (fn [id]
           ['[*
              {:kr/join ...}]
            id]))

       (let [r (f/subscribe [::recursive-* id])]
         (is (= @r
                {:system/uuid    (second id)
                 :kr/name        "name"
                 :kr/description "description"
                 :kr/join        {:system/uuid (second join)
                                  :kr/name     "join"
                                  :kr/label    "label"
                                  :kr/join     [{:system/uuid (second n1)
                                                 :kr/name     "nested 1"}
                                                {:system/uuid (second n2)
                                                 :kr/name     "nested 2"
                                                 :kr/join     {:system/uuid    (second n3)
                                                               :kr/name        "nested 3"
                                                               :kr/description "leaf"}}]}}))))

     (testing "Result fn"
       (f/reg-pull ::result-fn
         (fn [id]
           [[:system/uuid
             :kr/name
             :kr/label]
            id])
         (fn [{id    :system/uuid
               name  :kr/name
               label :kr/label}]
           (str id name label)))

       (let [r (f/subscribe [::result-fn join])]
         (is (= @r ":joinjoinlabel"))))

     (testing "Pull query updates"
       (f/dispatch [::update-name join])
       (let [r (f/subscribe [::join id])]
         (is (= @r
                {:system/uuid    (second id)
                 :kr/name        "name"
                 :kr/description "description"
                 :kr/join        {:system/uuid (second join)
                                  :kr/name     "join updated"
                                  :kr/label    "label"}})))

       (let [r (f/subscribe [::result-fn join])]
         (is (= @r ":joinjoin updatedlabel")))))))

(deftest reg-comp-test
  (fix/run-test-sync

   (f/reg-pull ::q1
     (fn [id]
       [[:system/uuid
         :kr/name
         :kr/join]
        id]))

   (f/reg-pull ::q2
     (fn [q1-result]
       [[:system/uuid
         :kr/name]
        (get q1-result :kr/join)]))

   (f/reg-comp ::reg-comp [::q2 ::q1])

   (f/reg-event-db ::init
     (fn [db [_ id join]]
       (db/mergen db {:system/uuid (second id)
                      :kr/name     "test"
                      :kr/join     {:system/uuid (second join)
                                    :kr/name     "join"}})))

   (f/with-ref {:system/uuid [id join]}
     (f/dispatch [::init id join])
     (let [r (f/subscribe [::reg-comp id])]
       (is (= {:system/uuid (second join)
               :kr/name     "join"}
              @r))))))
