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

(t/deftest event-fsm-test
  (t/testing "Basic event FSM"
    (fix/run-test-sync

     (fsm/reg-fsm ::test
       (fn [self]
         {:id    self
          :start ::start
          :stop  #{::stop}
          :fsm   {::start {[::advance self] {:to ::stop}}}}))

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
          :fsm      {::start {[::event self] {:to ::stop}}}}))

     (f/reg-no-op ::event)

     (f/with-ref {:component/uuid [fsm/self]}
       (let [state (f/subscribe [::initial-dispatch self])]
         (is (= ::stop @state)))))))

(t/deftest fsm-stop-test
  (t/testing "Stop FSM"
    (fix/run-test-sync

     (fsm/reg-fsm ::stop-fsm
       (fn [self]
         {:id    self
          :start ::s1
          :stop  #{::stop ::error}
          :fsm   {::s1 {[::advance self] {:to ::s2}}
                  ::s2 {[::advance self] {:to ::s1}
                        [::stop self]    {:to ::stop}
                        [::error self]   {:to ::error}}}}))

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
  (fn [entity]
    (-> entity
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
          :fsm   {::running [entity {:to   ::elected
                                     :when ::threshold}]}}))

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

(t/deftest timeout-fsm-test
  (t/testing "FSM with timeout and dispatch"
    (fix/run-test-async

      (fsm/reg-fsm ::timeout-fsm
        (fn [self]
          {:id    self
           :start ::start
           :fsm   {::start  {[::advance self] {:to ::middle}}
                   ::middle {[::fsm/timeout self 1] {:to       ::finish
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
