(ns reflet.fsm-test
  (:require [cljs.spec.alpha :as s]
            [cljs.test :as t :refer [is]]
            [day8.re-frame.test :as rft]
            [re-frame.db :as dbr]
            [re-frame.registrar :as reg]
            [reagent.ratom :as r]
            [reflet.core :as f]
            [reflet.db :as db]
            [reflet.fixtures :as fix]
            [reflet.fsm :as fsm]))

(f/reg-event-fx ::next-state
  (constantly nil))

(t/deftest no-op-fsm-test
  (t/testing "no-op event FSM"
    (fix/run-test-sync

     (fsm/reg-fsm ::no-op
       (fn [self]
         {:ref self
          :fsm {nil nil}}))

     (f/reg-no-op ::advance)

     (f/with-ref {:cmp/uuid [fsm/self]}
       (let [state (f/sub [::no-op self])]
         (is (nil? @state))
         (f/disp [::advance self])
         (is (nil? @state)))))))

(t/deftest event-fsm-test
  (t/testing "Basic event FSM"
    (fix/run-test-sync

     (fsm/reg-fsm ::test
       (fn [self]
         {:ref  self
          :stop ::stop
          :fsm  {nil {[::advance self] ::stop}}}))

     (f/reg-no-op ::do-not-advance ::advance)

     (f/with-ref {:cmp/uuid [fsm/self]}
       (let [state (f/sub [::test self])]
         (is (nil? @state))
         (f/disp [::do-not-advance])
         (is (nil? @state))
         (f/disp [::advance self])
         (is (= ::stop @state))))))

  (t/testing "Advance stem match"
    (fix/run-test-sync

     (fsm/reg-fsm ::test
       (fn [self]
         {:ref  self
          :stop ::stop
          :fsm  {nil {[::advance self] ::stop}}}))

     (f/reg-no-op ::do-not-advance ::advance)

     (f/with-ref {:cmp/uuid [fsm/self]}
       (let [state (f/sub [::test self])]
         (is (nil? @state))
         (f/disp [::do-not-advance])
         (is (nil? @state))
         (f/disp [::advance self "more"])
         (is (= ::stop @state)))))))

(t/deftest idempotent-lifecycles-test
  (t/testing "Idempotent lifecycle"
    (fix/run-test-sync
     (fsm/reg-fsm ::idempotent
       (fn [self]
         {:ref self
          :fsm {nil nil}}))

     (f/with-ref {:cmp/uuid [fsm/self]}
       (letfn [(get-handler []
                 (reg/get-handler :global-interceptor [::idempotent self]))]

         (t/testing "start"
           (is (not (fsm/started? [::idempotent self])))
           (is (nil? (get-handler)))
           (let [state (f/sub [::idempotent self])]
             (is (fsm/started? [::idempotent self]))
             (is (get-handler))
             (is (nil? @state))

             (let [handler (get-handler)]
               ;; We would normally not call fsm/start! outside an
               ;; animation frame, but these tests are synchronous so
               ;; we are ok.
               (fsm/start! @dbr/app-db [::idempotent self])
               (is (fsm/started? [::idempotent self]))
               (is (= handler (get-handler))))

             (t/testing "stop"
               (r/dispose! state)
               (is (not (fsm/started? [::idempotent self])))
               (is (nil? (get-handler)))))))))))

(t/deftest fsm-lifecycle-test
  (t/testing "Start and stop events"
    (fix/run-test-sync

     (fsm/reg-fsm ::lifecycle
       (fn [self]
         {:ref  self
          :stop ::stop
          :fsm  {nil       {[::advance self] ::started}
                 ::started {[::advance self] ::failed-to-stop}}}))

     (f/reg-no-op ::do-not-advance ::advance)

     (f/with-ref {:cmp/uuid [fsm/self]}
       (f/disp [::fsm/start [::lifecycle self]])
       (is (fsm/started? [::lifecycle self]))
       (f/disp [::advance self])
       (let [state (f/sub [::lifecycle self])]
         (is (= ::started @state))
         (f/disp [::fsm/stop [::lifecycle self]])
         (is (not (fsm/started? [::lifecycle self])))
         (f/disp [::advance self])
         (is (= ::started @state))
         (is (not= ::failed-to-stop @state)))))))

(t/deftest fsm-start-in-state-test
  (t/testing "Start in state"
    (fix/run-test-sync

     (fsm/reg-fsm ::start-in-state
       (fn [self]
         {:ref self
          :to  ::starting
          :fsm {nil        nil
                ::starting {[::advance self] ::started}}}))

     (f/reg-no-op ::advance)

     (f/with-ref {:cmp/uuid [fsm/self]}
       (let [state (f/sub [::start-in-state self])]
         (is (= ::starting @state)))))))

(t/deftest fsm-initial-dispatch-test
  (t/testing "FSM initial dispatch"
    (fix/run-test-sync

     (fsm/reg-fsm ::initial-dispatch
       (fn [self]
         {:ref      self
          :stop     ::stop
          :dispatch [::event self]
          :fsm      {nil {[::event self] ::stop}}}))

     (f/reg-no-op ::event)

     (f/with-ref {:cmp/uuid [fsm/self]}
       (let [state (f/sub [::initial-dispatch self])]
         ;; Note in run-test-sync, all dispatches are via
         ;; dispatch-sync. Under normal conditions, you would not
         ;; immediately see the state transition to ::stop.
         (is (= ::stop @state)))))))

(t/deftest clause-dispatch-test
  (t/testing "FSM clause dispatch"
    (fix/run-test-sync

     (fsm/reg-fsm ::test
       (fn [self]
         {:ref  self
          :stop ::stop
          :fsm  {nil {[::advance self]
                      {:to       ::stop
                       :dispatch [::effect self]}}}}))

     (f/reg-event-db ::effect
       (fn [db event-v]
         (assoc db ::effect event-v)))

     (f/reg-sub ::effect
       (fn [db _]
         (get db ::effect)))

     (f/reg-no-op ::do-not-advance ::advance)

     (f/with-ref {:cmp/uuid [fsm/self]}
       (let [state  (f/sub [::test self])
             effect (f/sub [::effect])]
         (is (nil? @state))
         (is (nil? @effect))
         (f/disp [::do-not-advance])
         (is (nil? @state))
         (is (nil? @effect))
         (f/disp [::advance self])
         (is (= ::stop @state))
         (is (= @effect [::effect self])))))))

(t/deftest initial-dispatch-later-test
  (t/testing "FSM initial dispatch-later"
    (fix/run-test-async

     (fsm/reg-fsm ::test
       (fn [self]
         {:ref            self
          :stop           ::stop
          :dispatch-later {:dispatch [::effect self]
                           :ms       10}
          :fsm            {nil {[::effect self] ::stop}}}))

     (f/reg-event-db ::effect
       (fn [db event-v]
         (assoc db ::effect event-v)))

     (f/reg-sub ::effect
       (fn [db _]
         (get db ::effect)))

     (f/reg-no-op ::advance)

     (f/with-ref {:cmp/uuid [fsm/self]}
       (let [state  (f/sub [::test self])
             effect (f/sub [::effect])]
         (is (nil? @state))
         (is (nil? @effect))
         (rft/wait-for [::effect]
           (is (= @state ::stop))
           (is (= @effect [::effect self]))))))))

(t/deftest clause-dispatch-later-test
  (t/testing "FSM clause dispatch-later"
    (fix/run-test-async

     (fsm/reg-fsm ::test
       (fn [self]
         {:ref  self
          :stop ::stop
          :fsm  {nil {[::advance self]
                      {:to             ::stop
                       :dispatch-later {:dispatch [::effect self]
                                        :ms       10}}}}}))

     (f/reg-event-db ::effect
       (fn [db event-v]
         (assoc db ::effect event-v)))

     (f/reg-sub ::effect
       (fn [db _]
         (get db ::effect)))

     (f/reg-no-op ::advance)

     (f/with-ref {:cmp/uuid [fsm/self]}
       (let [state  (f/sub [::test self])
             effect (f/sub [::effect])]
         (is (nil? @state))
         (is (nil? @effect))
         (f/disp-sync [::advance self])
         (is (= ::stop @state))
         (rft/wait-for [::effect]
           (is (= @effect [::effect self]))))))))

(t/deftest fsm-stop-test
  (t/testing "Stop FSM"
    (fix/run-test-sync

     (fsm/reg-fsm ::stop-one
       (fn [self]
         {:ref  self
          :attr ::one
          :stop ::stop
          :fsm  {nil {[::advance self] ::stop}}}))

     (fsm/reg-fsm ::stop-vec
       (fn [self]
         {:ref  self
          :attr ::vec
          :stop [::stop]
          :fsm  {nil {[::advance self] ::stop}}}))

     (fsm/reg-fsm ::stop-set
       (fn [self]
         {:ref  self
          :attr ::set
          :stop #{::stop}
          :fsm  {nil {[::advance self] ::stop}}}))

     (fsm/reg-fsm ::stop-list
       (fn [self]
         {:ref  self
          :attr ::list
          :stop '(::stop)
          :fsm  {nil {[::advance self] ::stop}}}))

     (f/reg-no-op ::advance ::stop)

     (f/with-ref {:cmp/uuid [fsm/self]}
       (let [s1 (f/sub [::stop-one self])
             s2 (f/sub [::stop-vec self])
             s3 (f/sub [::stop-set self])
             s4 (f/sub [::stop-list self])]
         (f/disp [::advance self])
         (is (= ::stop @s1))
         (is (= ::stop @s2))
         (is (= ::stop @s3))
         (is (= ::stop @s4))
         (f/disp [::advance self])))))

  (t/testing "Stop FSM"
    (fix/run-test-sync

     (fsm/reg-fsm ::stop-fsm
       (fn [self]
         {:ref  self
          :stop [::stop ::error]
          :fsm  {nil  {[::advance self] ::s1}
                 ::s1 {[::advance self] ::s2
                       [::reset self]   nil}
                 ::s2 {[::advance self] ::s1
                       [::stop self]    ::stop
                       [::error self]   ::error}}}))

     (f/reg-no-op ::advance ::stop ::error ::reset)

     (f/with-ref {:cmp/uuid [fsm/self]}
       (let [state (f/sub [::stop-fsm self])]
         (is (reg/get-handler :global-interceptor [::stop-fsm self]))
         (is (nil? @state))
         (f/disp [::advance self])
         (is (= ::s1 @state))
         (f/disp [::advance self])
         (is (= ::s2 @state))
         (f/disp [::advance self])
         (is (= ::s1 @state))
         (f/disp [::reset self])
         (is (nil? @state))
         (f/disp [::advance self])
         (is (= ::s1 @state))
         (f/disp [::advance self])
         (is (= ::s2 @state))
         (f/disp [::stop self])
         (is (= ::stop @state))
         (f/disp [::advance self])
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
       (fn [self candidate]
         {:ref  self
          :stop ::done
          :fsm  {nil       {[::kick-off self] ::running}
                 ::running {[candidate] {:to       ::done
                                         :when     ::threshold
                                         :dispatch [::elected candidate]}}}}))

     (f/reg-no-op ::kick-off)

     (f/reg-event-db ::vote
       (fn [db [_ candidate]]
         (db/update-inn db [candidate :votes] inc)))

     (f/reg-event-db ::elected
       (fn [db [_ candidate]]
         (db/assoc-inn db [candidate :elected] true)))

     (f/reg-pull ::candidate
       (fn [ref]
         [[:votes :elected]
          ref]))

     (f/with-ref {:cmp/uuid [fsm/self]
                  :system/uuid    [test/candidate]}
       (let [state (f/sub [::election self candidate])
             c     (f/sub [::candidate candidate])]
         (is (nil? @state))
         (f/disp [::kick-off self])
         (is (= ::running @state))
         (f/disp [::vote candidate])
         (is (= ::running @state))
         (f/disp [::vote candidate])
         (is (= ::running @state))
         (f/disp [::vote candidate])
         (is (= ::done @state)))))))

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
         {:ref  self
          :stop ::done
          :fsm  {nil    {[self] [{:to   ::even
                                  :when ::even}
                                 {:to   ::odd
                                  :when ::odd}]}
                 ::odd  {[self] [{:to   ::done
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

     (f/with-ref {:cmp/uuid [fsm/self]}
       (let [state (f/sub [::number self])
             n     (f/sub [::n self])]
         (is (nil? @n))
         (is (nil? @state))
         (f/disp [::set self 1])
         (is (= @n 1))         
         (is (= ::odd @state))
         (f/disp [::set self 2])
         (is (= @n 2))
         (is (= ::even @state))
         (f/disp [::set self 3])
         (is (= @n 3))
         (is (= ::odd @state))
         (f/disp [::set self 4])
         (is (= @n 4))
         (is (= ::done @state))
         (f/disp [::set self 4])
         (is (= @n 4))
         (is (= ::done @state))
         (f/disp [::set self 5])
         (is (= @n 5))
         (is (= ::done @state)))))))

(t/deftest timeout-fsm-test
  (t/testing "FSM with timeout and dispatch"
    (fix/run-test-async

     (fsm/reg-fsm ::timeout-fsm
       (fn [self]
         {:ref  self
          :stop ::finish
          :fsm  {nil      {[::advance self] ::middle}
                 ::middle {[::fsm/timeout self 1]
                           {:to       ::finish
                            :dispatch [::timeout-success]}}}}))

     (f/reg-no-op ::advance ::timeout-success)

     (f/with-ref {:cmp/uuid [fsm/self]}
       (let [state (f/sub [::timeout-fsm self])]
         (is (nil? @state))
         (f/disp-sync [::advance self])
         (is (= ::middle @state))
         (rft/wait-for [::timeout-success]
           (is (= ::finish @state))))))))

(t/deftest entity-timeout-fsm-test
  (t/testing "Entity FSM with timeout and dispatch"
    (fix/run-test-async

     (fsm/reg-fsm ::timeout-fsm
       (fn [self]
         {:ref  self
          :stop ::finish
          :fsm  {nil      {[::advance self] ::middle}
                 ::middle {[self] {:to   ::start
                                   :when ::threshold}

                           [::fsm/timeout self 1 ::middle]
                           {:to       ::finish
                            :dispatch [::timeout-success]}}}}))

     (f/reg-no-op ::advance ::timeout-success)

     (f/with-ref {:cmp/uuid [fsm/self]}
       (let [state (f/sub [::timeout-fsm self])]
         (is (nil? @state))
         (f/disp-sync [::advance self])
         (is (= ::middle @state))
         (rft/wait-for [::timeout-success]
           (is (= ::finish @state))))))))

(t/deftest recursive-fsm-test
  (t/testing "Recursive FSM definition"
    (fix/run-test-sync

     (fsm/reg-fsm ::recursive
       (fn [self]
         {:ref  self
          :stop ::stop
          :fsm  {{nil  {[::advance self] ::s1}
                  ::s1 {[::advance self] ::s2}
                  ::s2 {[::advance self] ::s3}} {[::restart self] nil}

                 ::s3 {[::advance self] ::stop}}}))

     (f/reg-no-op ::advance ::restart)

     (f/with-ref {:cmp/uuid [fsm/self]}
       (let [state (f/sub [::recursive self])]
         (is (nil? @state))
         (f/disp [::restart self])
         (is (nil? @state))

         (f/disp [::advance self])
         (is (= ::s1 @state))
         (f/disp [::restart self])
         (is (nil? @state))

         (f/disp [::advance self])
         (is (= ::s1 @state))
         (f/disp [::advance self])
         (is (= ::s2 @state))
         (f/disp [::restart self])
         (is (nil? @state))

         (f/disp [::advance self])
         (is (= ::s1 @state))
         (f/disp [::advance self])
         (is (= ::s2 @state))
         (f/disp [::advance self])
         (is (= ::s3 @state))
         (f/disp [::restart self])
         (is (= ::s3 @state))
         (f/disp [::advance self])
         (is (= ::stop @state))
         (is (not (fsm/started? [::recursive self]))))))))
