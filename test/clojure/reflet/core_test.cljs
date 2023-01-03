(ns reflet.core-test
  (:require [cljs.test :refer [deftest testing is are use-fixtures]]
            [re-frame.registrar :as reg]
            [reagent.ratom :as r]
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

(deftest with-ref-transient-test
  ;; Fake a reactive context to simulate what the metadata should look
  ;; like in situ with transient refs.
  (fix/fake-reactive-context
   (testing "transient is default true on fresh refs"
     (is (f/with-ref {:system/uuid [t]}
           (db/transient? t)))

     (is (f/with-ref {:system/uuid [p]
                      :meta        {:transient false}}
           (false? (db/transient? p))))
     (let [props {:p [:system/uuid "a"]}]
       (is (f/with-ref {:system/uuid [p] :in props}
             (false? (db/transient? (:p props)))))))

   (testing "Arbitrary meta data"
     (is (= (f/with-ref {:system/uuid [m]
                         :meta        {:provisional true}}
              (db/ref-meta m))
            {:provisional true
             :transient   true})))))

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
    (is (= (f/with-ref {:system/uuid [self target]
                        :cmp/uuid    [local]}
             [self target local])
           [[:system/uuid :self] [:system/uuid :target] [:cmp/uuid :local]]))
    (is (= (f/with-ref {:system/uuid [self target]
                        :cmp/uuid    [local]}
             [self target local])
           [[:system/uuid :self] [:system/uuid :target] [:cmp/uuid :local]]))
    (is (= (f/with-ref {:system/uuid [search/self search/target]
                        :cmp/uuid    [:search/local]}
             [self target local])
           [[:system/uuid :search/self]
            [:system/uuid :search/target]
            [:cmp/uuid :search/local]])))

  (let [props {}]
    (testing "props local re-binding"
      (is (= (f/with-ref {:system/uuid [self] :in props}
               [self props])
             [[:system/uuid :self] {:self [:system/uuid :self]}]))
      (is (= (f/with-ref {:system/uuid [self target]
                          :cmp/uuid    [local]
                          :in          props}
               [self target local props])
             [[:system/uuid :self]
              [:system/uuid :target]
              [:cmp/uuid :local]
              {:self   [:system/uuid :self]
               :target [:system/uuid :target]
               :local  [:cmp/uuid :local]}]))
      (is (= (f/with-ref {:system/uuid [search/self search/target]
                          :cmp/uuid    [:search/local ::state]
                          :in          props}
               [self target local state props])
             [[:system/uuid :search/self]
              [:system/uuid :search/target]
              [:cmp/uuid :search/local]
              [:cmp/uuid ::state]
              {:search/self   [:system/uuid :search/self]
               :search/target [:system/uuid :search/target]
               :search/local  [:cmp/uuid :search/local]
               ::state        [:cmp/uuid ::state]}]))))

  (testing "props external re-binding"
    (let [props {:search/self "external"}]
      (is (= (f/with-ref {:system/uuid [search/self search/target]
                          :cmp/uuid    [:search/local]
                          :in          props}
               [self target local props])
             ["external"
              [:system/uuid :search/target]
              [:cmp/uuid :search/local]
              {:search/self   "external"
               :search/target [:system/uuid :search/target]
               :search/local  [:cmp/uuid :search/local]}]))))

  (testing "props local fresh binding"
    ;; Note that unlike the previous two tests, props is not being
    ;; re-bound, it is bound fresh.
    (is (= (f/with-ref {:system/uuid [self]
                        :in          props}
             [self props])
           [[:system/uuid :self] {:self [:system/uuid :self]}])))

  (testing "map destructuring bindings"
    (is (= (f/with-ref {:system/uuid {sym :binding}
                        :in          props}
             [sym props])
           [[:system/uuid :binding] {:binding [:system/uuid :binding]}]))
    (is (= (f/with-ref {:system/uuid {sym ::binding}
                        :in          props}
             [sym props])
           [[:system/uuid ::binding] {::binding [:system/uuid ::binding]}]))
    (is (= (f/with-ref {:system/uuid {sym1 :b1
                                      sym2 :b2}
                        :in          props}
             [sym1 sym2 props])
           [[:system/uuid :b1]
            [:system/uuid :b2]
            {:b1 [:system/uuid :b1]
             :b2 [:system/uuid :b2]}]))))

(deftest random-ref-cofx-test
  (let [f (reg/get-handler :cofx ::f/with-ref)]
    (is (= (f {:db {}} {:system/uuid [:search/self :search/target]
                        :cmp/uuid    [:search/local]})
           {:db          {}
            ::f/with-ref {:search/self   [:system/uuid :search/self]
                          :search/target [:system/uuid :search/target]
                          :search/local  [:cmp/uuid :search/local]}}))))

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
     (f/disp [::init id join n1 n2 n3])

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

       (let [r (f/sub [::join id])]
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

       (let [r (f/sub [::attr id])]
         (is (= @r "name"))))

     (testing "Wildcard query"
       (f/reg-pull ::wildcard
         (fn [id]
           ['[* {:kr/join [*]}]
            id]))

       (let [r (f/sub [::wildcard id join])]
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

       (let [r (f/sub [::link-q])]
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

       (let [r (f/sub [::recursive... id])]
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

       (let [r (f/sub [::recursive-2 id])]
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
           ['[* {:kr/join ...}]
            id]))

       (let [r (f/sub [::recursive-* id])]
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

       (let [r (f/sub [::result-fn join])]
         (is (= @r ":joinjoinlabel"))))

     (testing "Pull query updates"
       (f/disp [::update-name join])
       (let [r (f/sub [::join id])]
         (is (= @r
                {:system/uuid    (second id)
                 :kr/name        "name"
                 :kr/description "description"
                 :kr/join        {:system/uuid (second join)
                                  :kr/name     "join updated"
                                  :kr/label    "label"}})))

       (let [r (f/sub [::result-fn join])]
         (is (= @r ":joinjoin updatedlabel")))))))

(def pull-fx-db
  (atom []))

(defmethod f/pull-fx ::test
  [{v :val} {:keys [ref]}]
  (swap! pull-fx-db conj [v ref]))

(deftest reg-pull-effects-test
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

   (f/with-ref {:system/uuid [id join n1 n2 n3]}
     (f/disp [::init id join n1 n2 n3])

     (reset! pull-fx-db [])
     (testing "Query effects"
       (f/reg-pull ::join
         (fn [id]
           [([:system/uuid
              :kr/name
              (:kr/description {:id ::test :val ::e2})
              ({:kr/join ([:system/uuid
                           :kr/name
                           :kr/label
                           {:kr/join ([:kr/name] {:id ::test :val ::e5})}]
                          {:id ::test :val ::e4})}
               {:id ::test :val ::e3})]
             {:id ::test :val ::e1})
            id]))

       (let [r (f/sub [::join id])]
         (is (= @r
                {:system/uuid    (second id)
                 :kr/name        "name"
                 :kr/description "description"
                 :kr/join        {:system/uuid (second join)
                                  :kr/name     "join"
                                  :kr/label    "label"
                                  :kr/join     [{:kr/name "nested 1"}
                                                {:kr/name "nested 2"}]}}))
         (is (= @pull-fx-db
                [[::e1 [:system/uuid :id]]
                 [::e2 [:system/uuid :id]]
                 [::e3 [:system/uuid :id]]
                 [::e4 [:system/uuid :join]]
                 [::e5 [:system/uuid :n1]]
                 [::e5 [:system/uuid :n2]]]))))

     (reset! pull-fx-db [])
     (testing "Single attribute pull effects"
       (f/reg-pull ::attr
         (fn [id]
           [({:kr/join ([:system/uuid
                         :kr/name
                         :kr/label]
                        {:id ::test :val ::e2})}
             {:id ::test :val ::e1})
            id]))

       (let [r (f/sub [::attr id])]
         (is (= @r
                {:system/uuid (second join)
                 :kr/name     "join"
                 :kr/label    "label"}))
         (is (= @pull-fx-db
                [[::e1 [:system/uuid :id]]
                 [::e2 [:system/uuid :join]]]))))

     (reset! pull-fx-db [])
     (testing "link entry pull effects"
       (f/reg-pull ::link
         (fn [id]
           [({::link ([:system/uuid
                       :kr/name
                       :kr/description]
                      {:id ::test :val ::e2})}
             {:id ::test :val ::e1})]))

       (let [r (f/sub [::link id])]
         (is (= @r
                {:system/uuid    (second id)
                 :kr/name        "name"
                 :kr/description "description"}))
         ;; Note: there is no entity reference at the top level link
         ;; query context
         (is (= @pull-fx-db
                [[::e1 nil]
                 [::e2 [:system/uuid :id]]])))))))

(deftest reg-comp-test
  (fix/run-test-sync

   (f/reg-pull ::q1
     ;; Query returns first reference in join.
     (fn [id]
       [[:system/uuid
         :kr/name
         :kr/join]
        id])
     (comp first :kr/join))

   (f/reg-pull ::q2
     (fn [q1-result]
       [[:system/uuid
         :kr/name]
        q1-result]))

   (f/reg-comp ::reg-comp [::q2 ::q1])

   (f/reg-event-db ::init
     (fn [db [_ id1 id2 id3]]
       (db/mergen db {:system/uuid (second id1)
                      :kr/name     "1"
                      :kr/join     [{:system/uuid (second id2)
                                     :kr/name     "2"}
                                    {:system/uuid (second id3)
                                     :kr/name     "3"}]})))

   (f/with-ref {:system/uuid [id1 id2 id3]}
     (f/disp [::init id1 id2 id3])
     (let [r (f/sub [::reg-comp id1])]
       (is (= {:system/uuid (second id2)
               :kr/name     "2"}
              @r))))))
