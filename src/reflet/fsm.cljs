(ns reflet.fsm
  "Provides finite state machine DSL and implementation.

  The fsm implementation is based on an entity model, where any entity
  in the db can be transitioned through allowed states given defined
  inputs. This means both domain entities as well as component
  entities can be used as fsms.

  Each fsm is defined declaratively. For example:

  (f/reg-fsm ::review
    (fn [self]
      {:id    self
       :attr  :kr/votes
       :start ::open
       :stop  [::accepted]
       :fsm   {::open     {[:voted self] ::review}
               ::review   {[self] {:to       ::review
                                   :when     ::review-threshold
                                   :dispatch [:notify self]}}
               ::decision {[:accepted self] ::accepted
                           [:revisit self]  {:to      ::review
                                             :dipatch [:reset-votes self]}}}}))

  Here, the `:fsm` attribute defines a transition map, mapping fsm
  states to allowed transitions. All fsm states are global and must be
  namespaced.

  Two types of transition maps can be defined based on the type of the
  transition input. Inputs can be entities, or dispatched re-frame
  events.

  Event transitions:

  These are expressed as a map, or a vector of maps. Each transition
  set maps re-frame event vectors with transition states.

  Entity transitions:

  These are expressed as a vector, with only a single allowed
  transition per state (though the DSL could be easily extended to
  allow multiple entity transitions in the future without
  breaking). Each entity transition maps an input entity ref to the
  next fsm state.

  In addition to specifying the next state in the transition, both
  event transitions and entity transitions can specify optional
  conditional clauses, and dispatch clauses.

  Other attributes that define an fsm:

  `:id`         - A  db reference to the fsm entity being advanced
                  through states

  `:start`      - The default starting state if the entity is not
                  already in a state

  `:attr`       - Optional: the entity attribute where the state is
                  stored, when not provided `::state` is used
  
  `:stop`       - Optional: the state where the fsm will be stopped

  `:return`     - An optional pull spec run as the return value of the
                  resultant FSM subscription. By default, the fsm
                  subscription returns a simple attribute query on
                  the state attribute specified by `:attr`. The
                  query is always run agains the FSM `:id` as the
                  root reference.

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
   (s/cat :event-id keyword?
          :event-args (s/* any?))))

(s/def ::dispatch
  (s*/any-cardinality ::event :coerce-many true))

(s/def ::stop
  (s/coll-of ::state :kind set?))

(s/def ::transition-to-complex
  (s/keys :req-un [::to]
          :opt-un [::dispatch ::when]))

(s/def ::transition-to-simple keyword?)

(s/def ::transition-to
  (s*/conform-to
    (s/or :simple ::transition-to-simple
          :complex ::transition-to-complex)
    (fn [[t form]]
      (case t
        :simple  {:to form}
        :complex form))))

(s/def ::transition-to-any
  (s*/any-cardinality ::transition-to :coerce-many true))

(s/def ::event-transition
  (s/tuple ::event ::transition-to-any))

(s/def ::entity-transition
  (s/tuple (s/coll-of ::s*/ref) ::transition-to-any))

(s/def ::transition
  (s/and (s/or :event ::event-transition
               :entity ::entity-transition)
         (s/conformer second)))

(s/def ::at-most-one-timeout-transition
  (fn [transitions]
    (->> transitions
         (map first)
         (filter (comp #{::timeout} first))
         (count)
         (>= 1))))

(s/def ::entity-vec
  (s/coll-of ::s*/ref :kind vector?))

(s/def ::at-most-one-entity-transition
  (fn [transitions]
    (->> transitions
         (map first)
         (filter (partial s/valid? ::entity-vec))
         (count)
         (>= 1))))

(defn- compile-smallest-match
  [transitions]
  (->> transitions
       (keys)
       (map count)
       (apply min)
       (vary-meta transitions assoc :smallest-match)))

(defn- find-timeout
  [transitions]
  (->> transitions
       (keys)
       (filter (comp #{::timeout} first))
       (first)))

(defn- compile-timeout
  [transitions]
  (if-let [event-v (find-timeout transitions)]
    (vary-meta transitions assoc :timeout event-v)
    transitions))

(defn- compile-event-transitions
  [transitions]
  (-> (into {} transitions)
      (compile-smallest-match)
      (compile-timeout)))

(defn- compile-entity-transitions
  [transitions]
  (-> (into {} transitions)
      (compile-timeout)))

(s/def ::event-transitions
  (s/and (s/conformer seq)
         (s/coll-of ::event-transition)
         ::at-most-one-timeout-transition
         (s/conformer compile-event-transitions)))

(s/def ::entity-transitions
  (s/and (s/conformer seq)
         (s/coll-of ::transition :min-count 1 :max-count 2)
         ::at-most-one-timeout-transition
         ::at-most-one-entity-transition
         (s/conformer compile-entity-transitions)))

(s/def ::transitions
  (s/or :event  ::event-transitions
        :entity ::entity-transitions))

(s/def :state-map/fsm
  (s/map-of ::state ::transitions))

(s/def ::fsm
  (s/keys :req-un [::id ::start :state-map/fsm]
          :opt-un [::stop ::dispatch]))

(defn- parse
  [fsm]
  (s*/assert! ::fsm fsm))

(defn- match-clause
  [x clauses]
  (->> clauses
       (remove nil?)
       (filter (fn [{c :when}] (or (not c) (s/valid? c x))))
       (first)))

(defn- keep-matching?
  [transitions event]
  (let [n (:smallest (meta transitions))]
    (and (seq event)
         (or (not n)
             (<= n (count event))))))

(defn- match-transition
  "Find a transition that matches a sub sequence of the event."
  [event transitions]
  (when (keep-matching? transitions event)
    (or (get transitions event)
        (recur (pop event) transitions))))

(defn match-event
  [event transitions]
  (some->> transitions
           (match-transition event)
           (match-clause event)))

(defn- event-transition!
  "Given a transition map and an event, returns the next fsm state if
  there is a valid transition, `nil` otherwise. Event transition
  `:when` clause is optionally applied."
  [event transitions]
  (when-let [clause (match-event event transitions)]
    (let [{to       :to
           dispatch :dispatch} clause]
      (when dispatch
        (doseq [e dispatch]
          (f/dispatch e)))
      to)))

(defn- get-entities
  [db refs]
  (map (partial db/getn db) refs))

(defn- get-entity-transition
  "Handles entity transitions and timeouts."
  [db event transitions]
  (or (find transitions event)          ; Only timeouts would match
      (some (fn [[[k :as v] clauses]]
              (when-not (= k ::timeout)
                [(get-entities db v) clauses]))
            transitions)))

(defn- entity-transition!
  "Given a transition map and a db, returns the next fsm state if there
  is a valid transition, `nil` otherwise. Entity transitions will
  always have a `:when` spec predicate."
  [db event transitions]
  (let [[input clauses] (get-entity-transition db event transitions)]
    (when-let [clause (match-clause input clauses)]
      (let [{to       :to
             dispatch :dispatch} clause]
        (when dispatch
          (doseq [e dispatch]
            (f/dispatch e)))
        to))))

(defn- maybe-vec
  "Event must be a vector for efficient partial matching. It should
  almost always be one, but don't fail edge cases where it is not."
  [event]
  (cond-> event
    (not (vector? event)) vec))

(defn- next-state
  "Returns next state if there is a valid transition, `nil` otherwise."
  [fsm db event*]
  (let [{state-map :fsm
         :keys     [id attr start]
         :or       {attr ::state}} fsm

        event           (maybe-vec event*)
        current-state   (db/get-inn db [id attr] start)
        [t transitions] (get state-map current-state)]
    (case t
      :event  (event-transition! event transitions)
      :entity (entity-transition! db event transitions)
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
  (when-let [[_ ref ms :as event-v] (get-timeout fsm state)]
    (assert-timeout! fsm ref)
    (clear-timeout! timeout)
    (as-> #(f/dispatch event-v) %
      (js/setTimeout % ms)
      (reset! timeout %))))

(defn advance
  "Given a parsed fsm, a db, and an event, advances the fsm. Else,
  no-op. Do not write to unmounted transient entities."
  [{:keys [id attr stop start fsm-v]
    :or   {attr ::state}
    :as   fsm} timeout db event]
  (if (db/transient-unmounted? id)
    db
    (if-let [state (next-state fsm db event)]
      (do (when (and stop (contains? stop state))
            (f/dispatch [::stop fsm-v]))
          (set-timeout! fsm timeout state)
          (db/assoc-inn db [id attr] state))
      db)))

(f/reg-event-fx ::timeout
  [db/inject-index i/add-global-interceptors]
  (constantly nil))

;;;; Effects

(defn- fsm-spec
  [fsm-v]
  (let [[id & args] fsm-v]
    (or (some-> (reg/get-handler ::fsm-fn id)
                (apply args)
                (assoc :fsm-v fsm-v))
        (throw (ex-info "No FSM handler" {:id id})))))

(defn- initial-dispatch!
  [{:keys [dispatch]}]
  (doseq [event dispatch]
    (f/dispatch event)))

(defn started?
  [fsm-v]
  (i/global-interceptor-registered? fsm-v))

(defn start!
  [fsm-v]
  ;; Must manually set timeout on start state.
  (let [fsm (parse (fsm-spec fsm-v))]
    (when (and fsm (not (started? fsm-v)))
      (let [timeout (atom nil)]
        (set-timeout! fsm timeout (:start fsm))
        (->> (partial advance fsm timeout)
             (f/enrich)
             (i/reg-global-interceptor fsm-v)))
      (initial-dispatch! fsm))))

(defn stop!
  [fsm-v]
  (i/clear-global-interceptor fsm-v))

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

(defn- fsm-reaction-handler
  [_ fsm-v]
  (let [{ref         :id
         attr        :attr
         start-state :start
         pull-expr   :return
         :or         {attr ::state}
         :as         fsm} (fsm-spec fsm-v)]
    (if ref
      (letfn [(expr-fn [ref] [(or pull-expr attr) ref])
              (result-fn [r] (or r start-state))]
        (start! fsm-v)
        (-> {:on-dispose #(stop! fsm-v)}
            (db/pull-reaction expr-fn fsm-v)
            (db/result-reaction result-fn fsm-v)))
      (r/reaction nil))))

(defn reg-fsm
  [id fsm-fn]
  (reg/register-handler ::fsm-fn id fsm-fn)
  (f/reg-sub-raw id fsm-reaction-handler))
