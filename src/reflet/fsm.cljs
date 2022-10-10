(ns reflet.fsm
  "Provides finite state machine DSL and implementation.

  This fsm implementation is based on an entity model, where any
  entity in the db can be transitioned through allowed states given
  defined inputs. This means both domain entities as well as component
  entities can be used as fsms.

  Each fsm is defined declaratively. For example:

  (f/reg-fsm ::review
    (fn [self]
      {:ref  self
       :attr :kr/votes
       :stop #{::accepted ::cancelled}
       :fsm  {nil        {[:voted self] ::review}
              ::review   {[self] {:to       ::review
                                  :when     ::review-threshold
                                  :dispatch [:notify self]}}
              ::decision {[:accepted self]          ::accepted
                          [:revisit self]           {:to      ::review
                                                     :dipatch [:reset self]}
                          [::fsm/timeout self 1000] [:to ::cancelled]}}}))

  This spec requires the following attributes:

  `:ref`
            A db reference to the FSM entity being advanced through
            states

  `:fsm`
            Defines the allowed states of the FSM, mapping each state
            to a set of allowed transitions.

  The following optional attributes are also supported:

  `:attr`
            The entity attribute where the state is stored. If not
            provided `::state` is used

  `:stop`
            One or more states which when reached will stop the fsm.

  `:return`
            A pull spec run as the return value of the resultant FSM
            subscription. By default, the fsm subscription returns a
            simple attribute something more query on the state
            attribute specified by `:attr`. The query is always run
            agains the FSM `:ref` as the root reference.

  `:dispatch`
            One or more events to dispatch immediately after starting
            the FSM

  `:dispatch-later`
            Same, but conforms to the re-frame `:dispatch-later` fx
            syntax.

  Like re-frame events and subscriptions, FSMs are uniquely identified
  by a vector that starts with their `reg-fsm` id, followed by their
  arguments: `[::review self]`.

  FSMs are implemented via interceptors that will advance the FSM on
  any recieved re-frame event.

  A running FSM will throw an error if it ever reaches a state that is
  not defined in either the `:fsm` state map, or the set of `:stop`
  states. You can define a state with no transitions by mapping the
  state to `nil`:

  {:fsm {::going-nowhere   nil
         ::going-somewhere {[:event ref] ::going-nowhere}}}

  Note this does NOT mean that `::going-nowhere` will transition to
  the `nil` state, but rather that `::going-nowhere` has no
  transitions defined.

  This will effectively pause the FSM, without actually turning off
  its interceptor. To prevent the interceptor from running needlessly
  you would normally declare `::going-nowhere` as a stop state
  instead:

  {:stop #{::going-nowhere}
   :fsm  {::going-somewhere {[:event ref] ::going-nowhere}}}

  The one use case for mapping a state to `nil`, rather than declaring
  it as a stop state, is if you want the FSM to sit and do nothing
  until some other process has manually set the state attribute of the
  FSM entity in the db. At this point the FSM would immediately
  advance through the new state's defined transitions, starting with
  the same event that manually set the FSM state. But this use case is
  pretty niche.

  Each transition for a given state is a map between an input, and one
  or more output clauses.

  There three types of transitions currently implemented, each
  corresponding to their input type:

  1. Event transitions
  2. Entity transitions
  3. Timeout transitions

  Event transitions match a recieved event against a set of event
  stems. Each stem either matches the event exactly, or an event with
  additional args. If more than one input stem would match, then the
  longest stem is chosen. For example, given the recieved event:

  [:voted self first-pref second-pref]

  and the set of input stems:

  [:voted self]
  [:voted self first-pref]

  Then matching stem would be:

  [:voted self first-pref]

  Entity transitions match the state of their input entities against
  the conditionals defined in their output clauses. The input entities
  are expressed as a vector of entity references, each reference being
  a tuple with a unique attribute and a uuid. To keep things fast, no
  joins are traversed when retrieving the entity state from the db:
  you only get a flat map.

  Timeout transitions are just like event transitions, except the first
  three positional elements of their event vector are:

  [:reflet.fsm/timeout ref ms ...]

  where `ref` is an entity reference, and `ms` is the timeout duration
  in milliseconds.

  If the FSM arrives at a state that has a timeout in its transition
  map, and the timeout's `ref` is the same as the FSM `:ref` and
  specifies a `ms` duration, then the FSM will immediately set a
  timeout for that state that will fire within the designated time. If
  no other event causes the FSM to advance in this time, the timeout
  event is fired via `dispatch-sync` to advance the FSM through the
  timeout transition. Any timeouts that do not fire are cleaned up
  appropriately. If the timeout's `ref` is not the same as the FSM
  `:ref`, or if the timeout does not define a `ms` duration, then the
  timeout will be matched just like a regular event. This way you can
  listen for timeouts from other FSMs.

  Only one entity or timeout transition is allowed in a state's
  transition map. However, a state's transition map can have an
  arbitrary number of event transitions. If a state defines both an
  entity and event transitions, the event transitions will always be
  matched before the entity transition.

  All transitions define one or more output clauses. Each output
  clause be expresed in either simple or expanded form.

  1. Simple: Just a state keyword
  2. Complex: A map containing the following attributes:

  `:to`
            The next state to transition to [required]

  `:when`

            The id of a Clojure spec that must return s/valid? true
            for the transition input in order for the transition to
            fire. For event inputs, this is simply the full recieved
            event vector. For entity inputs, the entity references are
            pulled from the db, and passed to the Clojure spec.
            [optional]

  `:dispatch`
            An event vector to dispatch on a succesful transition
            [optional]

  `:dispatch-later`
            Same semantics as

  Once an FSM has been declared using `reg-fsm`, instances can be
  created via subscriptions. When created this way, an FSM's lifecycle
  is tied to the lifecycle of its subscription. When the subscription
  is disposed, the FSM is also stopped. Subscriptions are the
  preferred way to create FSMs.

  If you really need to start or stop and FSM during the event/fx
  phase of the application, you can do so using the `::start` and
  `::stop` event and fx handlers, with the caveat that an FSM can
  never be started or stopped while mutating the db at the same
  time. The FSM implementation actually has an interceptor that throws
  an error if this ever happens.

  When an FSM is started, its initial state will be whatever is
  referenced by its FSM `:ref` in the db. If no state exists, then it
  will be `nil`. It is usually a good idea to always define a `nil`
  transition, or the FSM will stop progressing.

  Because the FSM implementation is based on global interceptors that
  run every time, all the matching and lookup algorithms are written
  to be very fast."
  (:require [cinch.core :as util]
            [clojure.set :as set]
            [clojure.spec.alpha :as s]
            [re-frame.core :as f]
            [re-frame.db :as fdb]
            [re-frame.fx :as fx]
            [re-frame.registrar :as reg]
            [reagent.ratom :as r]
            [reflet.db :as db]
            [reflet.interceptors :as i]
            [reflet.trie :as t]
            [reflet.util.spec :as s*]))

(s/def ::ref ::s*/ref)
(s/def ::state (s/nilable qualified-keyword?))
(s/def ::when qualified-keyword?)
(s/def ::to ::state)
(s/def ::ms number?)

(s/def ::event-id
  (s/and keyword? (complement #{::timeout})))

(s/def ::event
  (s*/non-conformer
   (s/cat :event-id   ::event-id
          :more       (s/* any?))))

(s/def ::timeout
  (s*/non-conformer
   (s/cat :id     #{::timeout}
          :ref    ::s*/ref
          :ms     ::ms
          :more   (s/* any?))))

(s/def ::entity-vec
  (s/coll-of ::s*/ref :kind vector?))

(s/def ::dispatch
  (s*/any-cardinality ::event :coerce-many true))

(s/def :dispatch-later/dispatch ::event)

(s/def ::dispatch-later
  (s*/any-cardinality
   (s/keys :req-un [:dispatch-later/dispatch ::ms])
   :coerce-many true))

(s/def ::stop
  (s*/conform-to
    (s*/any-cardinality-conformed ::state)
    (fn [[t form]]
      (case t
        :one  #{form}
        :many (set form)))))

(s/def ::transition-to-expanded
  (s*/any-cardinality
   (s/keys :req-un [::to]
           :opt-un [::when ::dispatch ::dispatch-later])
   :coerce-many true))

(s/def ::transition-to
  (s*/conform-to
    (s/or :simple   ::state
          :expanded ::transition-to-expanded)
    (fn [[t form]]
      (case t
        :simple   [{:to form}]
        :expanded form))))

(s/def ::event-transition
  (s/tuple ::event ::transition-to))

(s/def ::entity-transition
  (s/tuple (s/coll-of ::s*/ref) ::transition-to))

(s/def ::timeout-transition
  (s/tuple ::timeout ::transition-to))

(s/def ::transition
  (s*/conform-to
    (s/or :event   ::event-transition
          :entity  ::entity-transition
          :timeout ::timeout-transition)
    (fn [[t form]]
      (with-meta form {:type t}))))

(def ^:private -type
  (comp :type meta))

(s/def ::max-one-timeout-transition
  (fn [transitions]
    (->> transitions
         (filter (comp #{:timeout} -type))
         (count)
         (>= 1))))

(s/def ::max-one-entity-transition
  (fn [transitions]
    (->> transitions
         (filter (comp #{:entity} -type))
         (count)
         (>= 1))))

(defn- compile-transitions
  [transitions]
  (letfn [(f [t [k v]]
            (t/add t k [k v]))]
    (-> (group-by -type transitions)
        (update :timeout ffirst)
        (update :entity first)
        (assoc :event (reduce f (t/trie) transitions))
        (set/rename-keys
         {:event  :event-trie
          :entity :entity-transition}))))

(s/def ::transitions
  (s/and (s/conformer seq)
         (s/coll-of ::transition)
         ::max-one-timeout-transition
         ::max-one-entity-transition
         (s/conformer compile-transitions)))

(s/def :state-map/fsm
  (s/map-of ::state (s/nilable ::transitions)))

(s/def ::fsm
  (s/keys :req-un [::ref :state-map/fsm]
          :opt-un [::stop ::dispatch ::dispatch-later]))

(defn- parse
  [fsm]
  (-> (s*/parse ::fsm fsm)
      (assoc :fsm-unparsed fsm)))

(defn- cond-clause
  [[input clauses]]
  (->> clauses
       (remove nil?)
       (filter (fn [{c :when}] (or (not c) (s/valid? c input))))
       (first)))

(defn- match-transition
  "Events are always matched before entities."
  [db event {trie     :event-trie
             entity-t :entity-transition}]
  (or (t/match trie event)
      (when entity-t
        (->> (partial db/getn db)
             (partial map)
             (update entity-t 0)))))

(defn- get-transition
  [{state-map :fsm
    {m :fsm}  :fsm-unparsed} state]
  (if (contains? state-map state)
    (get state-map state)
    (-> "FSM state not in state map"
        (ex-info {:state state :state-map m})
        (throw))))

(defn- match-clause
  "Given the current state of the fsm in the db, returns a matching
  clause. Care must be taken to handle the `nil` state."
  [{:keys [ref attr]
    :as   fsm} db event]
  (let [state (db/get-inn db [ref attr])]
    (some->> (get-transition fsm state)
             (match-transition db event)
             (cond-clause))))

(defn- get-timeout
  [fsm state]
  (some-> fsm
          (:fsm)
          (get state)
          (:timeout)))

(defn- clear-timeout!
  [timeout]
  (some-> @timeout (js/clearTimeout)))

(defn- set-timeout!
  [fsm timeout state]
  (when-let [[_ ref ms :as event-v] (get-timeout fsm state)]
    (when (= (:ref fsm) ref)
      (clear-timeout! timeout)
      (as-> #(f/dispatch-sync event-v) %
        (js/setTimeout % ms)
        (reset! timeout %)))))

(declare stop!)

(defn- cleanup!
  [{:keys [stop fsm-v]
    :as   fsm} timeout state]
  (if (contains? stop state)
    (stop! fsm-v)
    (set-timeout! fsm timeout state)))

(defn- fsm-dispatch!
  "Must happen after interceptor has been registered."
  [{:keys [dispatch dispatch-later]}]
  (doseq [event dispatch]
    (f/dispatch event))
  (doseq [event dispatch-later]
    (fx/dispatch-later event)))

(defn advance
  "Given a parsed fsm, a db, and an event, advances the fsm. Else,
  no-op. Do not write to unmounted transient entities."
  [{:keys [ref attr]
    :as   fsm} timeout db event]
  (if (db/transient-unmounted? ref)
    db
    (if-let [{to  :to
              :as clause} (match-clause fsm db event)]
      (do (fsm-dispatch! clause)
          (cleanup! fsm timeout to)
          (db/assoc-inn db [ref attr] to))
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
                (util/assoc-nil :attr ::state)
                (assoc :fsm-v fsm-v))
        (throw (ex-info "No FSM handler" {:fsm-v fsm-v})))))

(defn- set-first-timeout!
  [{:keys [ref attr]
    :as   fsm} timeout db]
  (->> (db/get-inn db [ref attr])
       (set-timeout! fsm timeout)))

(defn started?
  [fsm-v]
  (i/global-interceptor-registered? fsm-v))

(defn- start!
  [db fsm-v]
  (let [fsm (parse (fsm-spec fsm-v))]
    (when-not (started? fsm-v)
      (let [timeout (atom nil)]
        (set-first-timeout! fsm timeout db)
        (->> timeout
             (partial advance fsm)
             (f/enrich)
             (i/reg-global-interceptor fsm-v))
        (fsm-dispatch! fsm)))))

(defn- stop!
  [fsm-v]
  (i/clear-global-interceptor fsm-v))

(defn check-safe-usage
  [{{::keys [start stop]
     db     :db}   :effects
    {event :event} :coeffects}]
  (when (and db (or start stop))
    (-> "Cannot modify db when starting or stopping an FSM"
        (ex-info {:event event})
        (throw))))

(defn fsm-lifecycle-interceptor*
  "Guards against unsafe usage of FSM lifecycle effects. An FSM can
  never be started or stopped during the event/fx phase when the db is
  also being modified."
  [{{::keys [start stop]
     db-fx  :db} :effects
    {db-cfx :db} :coeffects
    :as          context}]
  (letfn [(f [fx]
            (check-safe-usage context)
            (conj [db-cfx] fx))]
    (cond-> context
      start           (update-in [:effects ::start] f)
      stop            (update-in [:effects ::stop] f)
      (or start stop) (update :effects dissoc :db))))

(def fsm-lifecycle-interceptor
  (f/->interceptor
   :id :add-global-interceptors
   :after fsm-lifecycle-interceptor*))

(f/reg-fx ::start
  (fn [[db fsm-v]]
    (start! db fsm-v)))

(f/reg-fx ::stop
  (fn [[_ fsm-v]]
    (stop! fsm-v)))

(f/reg-event-fx ::start
  fsm-lifecycle-interceptor
  (fn [_ [_ fsm-v]]
    {::start fsm-v}))

(f/reg-event-fx ::stop
  fsm-lifecycle-interceptor
  (fn [_ [_ fsm-v]]
    {::stop fsm-v}))

;;;; Subs

(defn- fsm-reaction-handler
  [db fsm-v]
  (let [{:keys [attr return]
         :or   {return attr}
         :as   fsm} (fsm-spec fsm-v)]
    (start! @db fsm-v)
    (db/pull-reaction {:on-dispose #(stop! fsm-v)}
                      #(do [return %])
                      fsm-v)))

(defn reg-fsm
  [id fsm-fn]
  (reg/register-handler ::fsm-fn id fsm-fn)
  (f/reg-sub-raw id fsm-reaction-handler))
