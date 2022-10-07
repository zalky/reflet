(ns reflet.fsm-test
  (:require [reflet.fixtures :as fix]
            [cljs.spec.alpha :as s]
            [cljs.test :as t :refer [is]]
            [day8.re-frame.test :as rf-test]
            [re-frame.registrar :as reg]
            [reflet.core :as f]
            [reflet.db :as db]
            [reflet.fsm :as fsm]
            [reflet.interceptors :as itor]))

(def interceptors
  [itor/add-global-interceptors db/inject-index])

(f/reg-event-fx ::next-state
  (constantly nil))

(t/deftest idempotent-test
  (t/testing "Idempotent"
    (fix/run-test-sync
     (fsm/reg-fsm ::idempotent
       (fn [self]
         {:id    self
          :start ::start
          :stop  #{::stop}
          :fsm   {}}))

     (f/with-ref {:component/uuid [fsm/self]}
       (letfn [(get-handler []
                 (reg/get-handler :global-interceptor [::idempotent self]))]

         (t/testing "start"
           (is (not (fsm/started? [::idempotent self])))
           (is (nil? (get-handler)))
           (let [state (f/subscribe [::idempotent self])]
             (is (fsm/started? [::idempotent self]))
             (is (get-handler))
             (is (= ::start @state))

             (let [handler (get-handler)]
               (fsm/start! [::idempotent self])
               (is (fsm/started? [::idempotent self]))
               (is (= handler (get-handler))))))

         (t/testing "stop"
           (fsm/stop! [::idempotent self])
           (is (not (fsm/started? [::idempotent self])))
           (is (nil? (get-handler)))))))))

(t/deftest no-op-fsm-test
  (t/testing "no-op event FSM"
    (fix/run-test-sync

     (fsm/reg-fsm ::no-op
       (fn [self]
         {:id    self
          :start ::start
          :fsm   {}}))

     (f/reg-no-op ::advance)

     (f/with-ref {:component/uuid [fsm/self]}
       (let [state (f/subscribe [::no-op self])]
         (is (= ::start @state))
         (f/dispatch [::advance self])
         (is (= ::start @state)))))))

(t/deftest event-fsm-test
  (t/testing "Basic event FSM"
    (fix/run-test-sync

     (fsm/reg-fsm ::test
       (fn [self]
         {:id    self
          :start ::start
          :stop  #{::stop}
          :fsm   {::start {[::advance self] ::stop}}}))

     (f/reg-no-op ::do-not-advance ::advance)

     (f/with-ref {:component/uuid [fsm/self]}
       (let [state (f/subscribe [::test self])]
         (is (= ::start @state))
         (f/dispatch [::do-not-advance])
         (is (= ::start @state))
         (f/dispatch [::advance self])
         (is (= ::stop @state)))))))

(t/deftest fsm-initial-dispatch-test
  (t/testing "FSM initial dispatch"
    (fix/run-test-sync

     (fsm/reg-fsm ::initial-dispatch
       (fn [self]
         {:id       self
          :start    ::start
          :dispatch [::event self]
          :fsm      {::start {[::event self] ::stop}}}))

     (f/reg-no-op ::event)

     (f/with-ref {:component/uuid [fsm/self]}
       (let [state (f/subscribe [::initial-dispatch self])]
         (is (= ::stop @state)))))))

(t/deftest transition-dispatch-test
  (t/testing "FSM transition dispatch"
    (fix/run-test-sync

     (fsm/reg-fsm ::test
       (fn [self]
         {:id    self
          :start ::start
          :stop  #{::stop}
          :fsm   {::start {[::advance self]
                           {:to       ::stop
                            :dispatch [::effect self]}}}}))

     (f/reg-event-db ::effect
       (fn [db event-v]
         (assoc db ::effect event-v)))

     (f/reg-sub ::effect
       (fn [db _]
         (get db ::effect)))

     (f/reg-no-op ::do-not-advance ::advance)

     (f/with-ref {:component/uuid [fsm/self]}
       (let [state  (f/subscribe [::test self])
             effect (f/subscribe [::effect])]
         (is (= ::start @state))
         (is (nil? @effect))
         (f/dispatch [::do-not-advance])
         (is (= ::start @state))
         (is (nil? @effect))
         (f/dispatch [::advance self])
         (is (= ::stop @state))
         (is (= @effect [::effect self])))))))

(t/deftest fsm-stop-test
  (t/testing "Stop FSM"
    (fix/run-test-sync

     (fsm/reg-fsm ::stop-fsm
       (fn [self]
         {:id    self
          :start ::s1
          :stop  #{::stop ::error}
          :fsm   {::s1 {[::advance self] ::s2}
                  ::s2 {[::advance self] ::s1
                        [::stop self]    ::stop
                        [::error self]   ::error}}}))

     (f/reg-no-op ::advance ::stop ::error)

     (f/with-ref {:component/uuid [fsm/self]}
       (let [state (f/subscribe [::stop-fsm self])]
         (is (reg/get-handler :global-interceptor [::stop-fsm self]))
         (is (= ::s1 @state))
         (f/dispatch [::advance self])
         (is (= ::s2 @state))
         (f/dispatch [::advance self])
         (is (= ::s1 @state))
         (f/dispatch [::advance self])
         (is (= ::s2 @state))
         (f/dispatch [::stop self])
         (is (= ::stop @state))
         (f/dispatch [::advance self])
         (is (= ::stop @state))
         (is (not (reg/get-handler :global-interceptor [::stop-fsm self]))))))))

(s/def ::threshold
  (fn [entities]
    (-> (first entities)
        (:votes)
        (> 2))))

(t/deftest entity-fsm-test
  (t/testing "Basic entity FSM"
    (fix/run-test-sync

     (fsm/reg-fsm ::election
       (fn [self entity]
         {:id    self
          :start ::running
          :stop  #{::elected}
          :fsm   {::running {[entity] {:to   ::elected
                                       :when ::threshold}}}}))

     (f/reg-event-db ::cast-vote
       (fn [db [_ entity]]
         (db/update-inn db [entity :votes] inc)))

     (f/with-ref {:component/uuid [fsm/self]
                  :system/uuid    [test/entity]}
       (let [state (f/subscribe [::election self entity])]
         (is (= ::running @state))
         (f/dispatch [::cast-vote entity])
         (is (= ::running @state))
         (f/dispatch [::cast-vote entity])
         (is (= ::running @state))
         (f/dispatch [::cast-vote entity])
         (is (= ::elected @state)))))))

(s/def ::even
  (comp even? ::n first))

(s/def ::odd
  (comp odd? ::n first))

(s/def ::four?
  #(= 4 (::n (first %))))

(t/deftest conditional-clause-test
  (t/testing "Multiple conditional clauses FSM"
    (fix/run-test-sync

     (fsm/reg-fsm ::number
       (fn [self]
         {:id    self
          :start ::odd
          :stop  #{::done}
          :fsm   {::odd  {[self] [{:to   ::done
                                   :when ::four?}
                                  {:to   ::even
                                   :when ::even}]}
                  ::even {[self] [{:to   ::done
                                   :when ::four?}
                                  {:to   ::odd
                                   :when ::odd}]}
                  ::done {[self] [{:to ::odd}]}}}))

     (f/reg-event-db ::set
       (fn [db [_ self n]]
         (db/assoc-inn db [self ::n] n)))

     (f/reg-pull ::n
       (fn [self]
         [::n self]))

     (f/with-ref {:component/uuid [fsm/self]}
       (let [state (f/subscribe [::number self])
             n     (f/subscribe [::n self])]
         (is (nil? @n))
         (is (= ::odd @state))
         (f/dispatch [::set self 1])
         (is (= @n 1))         
         (is (= ::odd @state))
         (f/dispatch [::set self 2])
         (is (= @n 2))
         (is (= ::even @state))
         (f/dispatch [::set self 3])
         (is (= @n 3))
         (is (= ::odd @state))
         (f/dispatch [::set self 4])
         (is (= @n 4))
         (is (= ::done @state))
         (f/dispatch [::set self 4])
         (is (= @n 4))
         (is (= ::done @state))
         (f/dispatch [::set self 5])
         (is (= @n 5))
         (is (= ::done @state)))))))

(t/deftest timeout-fsm-test
  (t/testing "FSM with timeout and dispatch"
    (fix/run-test-async

     (fsm/reg-fsm ::timeout-fsm
       (fn [self]
         {:id    self
          :start ::start
          :fsm   {::start  {[::advance self] ::middle}
                  ::middle {[::fsm/timeout self 1]
                            {:to       ::finish
                             :dispatch [::timeout-success]}}}}))

     (f/reg-no-op ::advance ::timeout-success)

     ;; Careful, if you don't explicitly mark entity references as
     ;; `:transient false`, with-ref will clean up app state as the
     ;; async test falls out of scope. This will nerf the async FSM
     ;; state, and cause the tests to hang..
     (f/with-ref {:component/uuid [fsm/self]
                  :meta           {:transient false}}
       (let [state (f/subscribe [::timeout-fsm self])]
         (is (= ::start @state))
         (f/dispatch-sync [::advance self])
         (is (= ::middle @state))
         (rf-test/wait-for [::timeout-success]
           (is (= ::finish @state))))))))

(t/deftest entity-timeout-fsm-test
  (t/testing "Entity FSM with timeout and dispatch"
    (fix/run-test-async

     (fsm/reg-fsm ::timeout-fsm
       (fn [self]
         {:id    self
          :start ::start
          :fsm   {::start  {[::advance self] ::middle}
                  ::middle {[self] {:to   ::start
                                    :when ::threshold}

                            [::fsm/timeout self 1]
                            {:to       ::finish
                             :dispatch [::timeout-success]}}}}))

     (f/reg-no-op ::advance ::timeout-success)

     ;; Careful, if you don't explicitly mark entity references as
     ;; `:transient false`, with-ref will clean up app state as the
     ;; async test falls out of scope. This will nerf the async FSM
     ;; state, and cause the tests to hang..
     (f/with-ref {:component/uuid [fsm/self]
                  :meta           {:transient false}}
       (let [state (f/subscribe [::timeout-fsm self])]
         (is (= ::start @state))
         (f/dispatch-sync [::advance self])
         (is (= ::middle @state))
         (rf-test/wait-for [::timeout-success]
           (is (= ::finish @state))))))))

