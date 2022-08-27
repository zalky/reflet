(ns reflet.fsm
  "Provides finite state machine DSL and implementation.

  The fsm implementation is based on an entity model, where any entity
  in the db can be transitioned through allowed states given defined
  inputs. This means both domain entities as well as component
  entities can be used as fsms.

  Each fsm is defined declaratively. For example:

  (let [fact #uuid 123]
    {:id         fact
     :state-attr :kr.epist/status
     :start      ::start
     :stop       ::accepted
     :fsm
     {::start       {:user/voted {:to ::provisional}}
      ::provisional [fact {:to       ::review
                           :when     ::review-threshold
                           :dispatch [:review/notify fact]}]
      ::review      {:user/accepted {:to ::accepted}
                     :user/clarification {:to ::provisional}}}})

  Here, the `:fsm` attribute defines a transition map, mapping fsm
  states to allowed transitions. All fsm states are global and must be
  namespaced.

  Two types of transition maps can be defined based on the type of the
  transition input. Inputs can be entities, or dispatched re-frame
  events.

  Event transitions:

  These are expressed as a map, with one or more allowed transitions
  per state. Each event transition maps an input re-frame event id to
  the next fsm state, `:to`, an optional `:when` condition, and an
  optional `:dispatch` event. The `:when` condition is a clojure spec
  keyword that, when present, is used as a predicate for when the
  transition can occur.

  Entity transitions:

  These are expressed as a vector, with only a single allowed
  transition per state (though the DSL could be easily extended to
  allow multiple entity transitions in the future without
  breaking). Each entity transition maps an input entity ref to the
  next fsm state, `:to`, a required `:when` condition, and optional
  `:dispatch` event. The `:when` condition is a clojure spec keyword
  that is used as a predicate for when the transition can occur. You
  cannot define an entity transition without a `:when` condition. The
  input to an entity transition can be any valid entity ref, including
  the fsm entity itself.

  Other attributes that define an fsm:

  `:id`         - A  db reference to the fsm entity being advanced
                  through states

  `:start`      - The default starting state if the entity is not
                  already in a state

  `:state-attr` - Optional: the entity attribute where the state is
                  stored, when not provided `::state` is used
  
  `:stop`       - Optional: the state where the fsm will be stopped

  An fsm can be started and stopped by dispatching the `::start` and
  `::stop` events, respectively."
  (:require [clojure.spec.alpha :as s]
            [re-frame.core :as f]
            [re-frame.registrar :as reg]
            [reagent.ratom :as r]
            [reflet.db :as db]
            [reflet.interceptors :as i]
            [reflet.util.spec :as s*]))

(s/def ::id ::s*/ref)
(s/def ::state qualified-keyword?)
(s/def ::when qualified-keyword?)
(s/def ::to ::state)
(s/def ::start ::state)

(s/def ::event
  (s*/identity-conformer
   (s/cat :event-id keyword? :event-args (s/* any?))))

(s/def ::dispatch
  (s*/any-cardinality ::event :coerce-many true))

(s/def ::stop
  (s/coll-of ::state :kind set?))

(s/def ::transition-to
  (s/keys :req-un [::to]
          :opt-un [::dispatch ::when]))

(defn compile-smallest-match
  [form]
  (->> form
       (keys)
       (map count)
       (apply min)
       (vary-meta form assoc :smallest)))

(defn compile-timeout
  [form]
  (->> form
       (some (fn [[[id ref ms] {to :to}]]
               (when (= id ::timeout)
                 {:timeout
                  {:ref ref
                   :ms  ms
                   :to  to}})))
       (vary-meta form merge)))

(s/def ::event-transitions
  (s*/conform-to
    (s/map-of ::event
              ::transition-to
              :conform-keys true)
    (comp compile-timeout
          compile-smallest-match)))

(s/def ::transitions
  (s/or :event  ::event-transitions
        :entity (s/tuple ::s*/ref ::transition-to)))

(s/def ::fsm (s/map-of ::state ::transitions))

(s/def ::fsm-spec
  (s/keys :req-un [::id ::start ::fsm]
          :opt-un [::stop ::dispatch]))

(defn- parse
  [fsm]
  (s*/assert! ::fsm-spec fsm))

(defn- keep-matching?
  [transitions event]
  (let [n (:smallest (meta transitions))]
    (and (seq event)
         (or (not n)
             (<= n (count event))))))

(defn match
  "Find a transition that matches a sub sequence of the event."
  [transitions event]
  (when (keep-matching? transitions event)
    (or (get transitions event)
        (recur transitions (butlast event)))))

(defn- event-transition!
  "Given a transition map and an event, returns the next fsm state if
  there is a valid transition, `nil` otherwise. Event transition
  `:when` clause is optionally applied."
  [transitions event]
  (when-let [{next-state :to
              spec       :when
              dispatch   :dispatch} (match transitions event)]
    (when dispatch
      (doseq [e dispatch]
        (f/dispatch e)))
    (when (or (not spec) (s/valid? spec event))
      next-state)))

(defn- entity-transition!
  "Given a transition map and a db, returns the next fsm state if there
  is a valid transition, `nil` otherwise. Entity transitions will
  always have a `:when` spec predicate."
  [[ref {next-state :to
         spec       :when
         dispatch   :dispatch}] db]
  (let [entity (db/getn db ref)]
    (when (s/valid? spec entity)
      (when dispatch
        (doseq [e dispatch]
          (f/dispatch e)))
      next-state)))

(defn next-state
  "Returns next state if there is a valid transition, `nil` otherwise."
  [fsm db event]
  (let [{id         :id
         state-attr :state-attr
         start      :start
         t-map      :fsm
         :or        {state-attr ::state}} fsm

        current-state   (db/get-inn db [id state-attr] start)
        [t transitions] (get t-map current-state)]
    (case t
      :event  (event-transition! transitions event)
      :entity (entity-transition! transitions db)
      nil)))

;;;;;;;;;;;; Timeout implementation ;;;;;;;;;;;;;;;;;;;

(defn- get-timeout
  [fsm state]
  (some-> fsm
          (:fsm)
          (get state)
          (second)
          (meta)
          (:timeout)))

(defn- assert-timeout!
  [fsm ref]
  (when-not (= (:id fsm) ref)
    (throw
     (ex-info
      "Only self timeouts allowed in FSMs"
      {:fsm fsm}))))

(defn- clear-timeout!
  [timeout]
  (some-> @timeout (js/clearTimeout)))

(defn- set-timeout!
  [{id :id :as fsm} timeout state]
  (when-let [{ms  :ms
              to  :to
              ref :ref} (get-timeout fsm state)]
    (assert-timeout! fsm ref)
    (clear-timeout! timeout)
    (as-> #(f/dispatch [::timeout id ms]) %
      (js/setTimeout % ms)
      (reset! timeout %))))

(defn advance
  "Given a parsed fsm, a db, and an event, advances the fsm. Else,
  no-op. Do not write to unmounted transient entities."
  [{:keys [id state-attr stop start fsm-v]
    :or   {state-attr ::state}
    :as   fsm} timeout db event]
  (if (db/transient-unmounted? id)
    db
    (if-let [state (next-state fsm db event)]
      (do (when (and stop (contains? stop state))
            (f/dispatch [::stop fsm-v]))
          (set-timeout! fsm timeout state)
          (db/assoc-inn db [id state-attr] state))
      db)))

(f/reg-event-fx ::timeout
  [db/inject-index i/add-global-interceptors]
  (constantly nil))

;;;; Effects

(defn- get-fsm
  [fsm-v]
  (let [[id & args] fsm-v]
    (or (some-> (reg/get-handler :fsm-fn id)
                (apply args)
                (assoc :fsm-v fsm-v))
        (throw (ex-info "No FSM handler" {:id id})))))

(defn started?
  [{id :id}]
  (i/global-interceptor-registered? id))

(defn initial-dispatch!
  [{:keys [dispatch]}]
  (doseq [event dispatch]
    (f/dispatch event)))

(defn start!
  [fsm-v]
  ;; Must manually set timeout on start state.
  (let [fsm (parse (get-fsm fsm-v))]
    (when (and fsm (not (started? fsm)))
      (let [timeout (atom nil)]
        (set-timeout! fsm timeout (:start fsm))
        (->> (partial advance fsm timeout)
             (f/enrich)
             (i/reg-global-interceptor (:id fsm))))
      (initial-dispatch! fsm))))

(defn stop!
  [fsm-v]
  (when-let [id (:id (get-fsm fsm-v))]
    (i/clear-global-interceptor id)))

(f/reg-fx ::start
  (fn [fsm-v]
    (start! fsm-v)))

(f/reg-fx ::stop
  (fn [fsm-v]
    (stop! fsm-v)))

;;;; Events

(f/reg-event-fx ::start
  ;; Starts the interceptor for the given fsm.
  (fn [_ [_ fsm-v]]
    {::start fsm-v}))

(f/reg-event-fx ::stop
  ;; Stops the interceptor for the given fsm.
  (fn [_ [_ fsm-v]]
    {::stop fsm-v}))

;;;; Subs

(defn fsm-reaction-handler
  [_ fsm-v]
  (let [{ref         :id
         start-state :start
         state-attr  :state-attr
         :or         {state-attr ::state}
         :as         fsm} (get-fsm fsm-v)
        query-v           [::pull ref]]
    (if ref
      (letfn [(expr [ref] [state-attr ref])
              (result [r] (or r start-state))]
        (start! fsm-v)
        (let [config {:on-dispose #(stop! fsm-v)}]
          (-> (db/pull-reaction query-v expr config)
              (db/result-reaction query-v result))))
      (r/reaction nil))))

(defn reg-fsm
  [id fsm-fn]
  (reg/register-handler :fsm-fn id fsm-fn)
  (f/reg-sub-raw id fsm-reaction-handler))

;;;; Materialized View

(defmulti render
  "Materialized view of the current fsm state. A `render` method must
  exist for each state defined in the fsm transition map. States are
  globally defined, and namespaced keywords are required. It is a good
  idea to define the fsm in the same namespace as the render methods."
  (fn [state _] state))

(defmethod render :default
  [state _]
  [:h2 (str "Undefined render state: " state)])
