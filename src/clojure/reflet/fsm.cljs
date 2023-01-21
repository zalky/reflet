(ns reflet.fsm
  "Provides finite state machine DSL and implementation.

  This FSM implementation is based on an entity model, where any
  entity in the db can be transitioned through allowed states given
  defined inputs. This means both domain entities as well as component
  entities can be used as FSMs.

  Each FSM is defined declaratively. For example:

  (f/reg-fsm ::review
    (fn [self user]
      {:ref  self
       :stop #{::accepted ::cancelled}
       :fsm  {nil        {[:voted self] ::review}
              ::review   {:* {:to       ::decision
                              :pull     [self]
                              :when     ::review-threshold
                              :dispatch [:notify user]}}
              ::decision {[::fsm/timeout self 1000] ::cancelled
                          [:accepted self]          {:to ::accepted}
                          [:revisit self]           ::review}}}))

  This spec requires the following attributes:

  `:ref`
            A db reference to the FSM entity being advanced through
            states.

  `:fsm`
            Defines the allowed states of the FSM, mapping each state
            to a set of allowed transitions.

  The following optional attributes are also supported:

  `:attr`
            The entity attribute where the FSM state is stored. If not
            provided `::state` is used.

  `:stop`
            One or more states which when reached will stop the
            FSM. The FSM interceptor will be removed, but the FSM
            state will also be set to the relevant state.

  `:return`
            The result returned by the FSM subscription. This is
            expressed as a pull spec that is run against the db with
            the FSM `:ref` as the root entity. The default pull spec
            returns a single attribute, which is the FSM state
            attribute (see the `:attr` option above).

  `:or`
            Advance the FSM to some default state if there is no
            state value in the db on init.

  `:dispatch`
            One or more events to dispatch immediately after starting
            the FSM.

  `:dispatch-later`
            Same, but conforms to the Re-frame `:dispatch-later` fx
            syntax.

  Like Re-frame events and subscriptions, FSMs are uniquely identified
  by a vector that starts with their `reg-fsm` id, followed by their
  arguments: `[::review self]`.

  FSMs are implemented via interceptors that will advance the FSM on
  any received Re-frame event.

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

  Each transition for a given state is a map between an input event,
  and one or more output clauses.

  There are two different types of event inputs currently defined:

  1. Event transitions
  2. Timeout transitions

  Event transitions match a received event against a set of event
  prefixes. Each prefix either matches the event exactly, or an event
  with additional args. If more than one event prefix would match,
  then the longest matching prefix is chosen. For example, given the
  received event:

  [:voted self first-pref second-pref]

  and the set of input prefixes:

  [:voted self]
  [:voted self first-pref]

  Then matching prefix would be:

  [:voted self first-pref]

  You can match any event with the special keyword :*.

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

  Only one wildcard or timeout transition is allowed in a state's
  transition map. Otherwise, a state's transition map can have an
  arbitrary number of event transitions. If a state defines both a
  wildcard and a set of event transitions, the event transitions will
  always be matched before the wildcard transition. As before: the
  most specific match will win.

  All transitions define one or more output clauses. Each output
  clause be expressed in either simple or expanded form.

  1. Simple: Just a state keyword
  2. Complex: A map containing the following attributes:

  `:to`
            The next state to transition to [required]

  `:when`
            A conditional clause that must return true for the
            transition to match. This can be either, a) the id of a
            Clojure spec that must return s/valid? `true`, or b) a
            Clojure function that returns a truthy value. By default,
            the input to either the spec or the function will be the
            received event that triggered the transition. If `:pull`
            is specified, a list of entities will be provided as input
            instead of the event. See the `:pull` option for more
            details. [optional]

  `:pull`
            A list of entity references. These entities are then
            passed as inputs to the :when conditional, in place of the
            trigger event. This allows you to build FSMs that use the
            state of other FSMs as inputs to their transitions.
            [optional]

  `:dispatch`
            An event vector to dispatch on a successful transition
            [optional]

  `:dispatch-later`
            Same semantics as `:dispatch-later` Re-frame fx.

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
  will be `nil`. You should always define a `nil` transition, or set
  the `:to` option to advance the FSM on startup. Otherwise, the FSM
  will never get out of the `nil` state.

  Because the FSM implementation is based on global interceptors that
  run every time, all the matching and lookup algorithms are written
  to be very fast."
  (:require [cinch.core :as util]
            [cinch.spec :as s*]
            [clojure.set :as set]
            [clojure.spec.alpha :as s]
            [re-frame.core :as f]
            [re-frame.db :as dbr]
            [re-frame.events :as events]
            [re-frame.fx :as fx]
            [re-frame.registrar :as reg]
            [re-frame.std-interceptors :as fitor]
            [reflet.db :as db]
            [reflet.interceptors :as i]
            [reflet.trie :as t]))

(s/def ::ref ::s*/ref)
(s/def ::state (s/nilable qualified-keyword?))
(s/def ::or qualified-keyword?)
(s/def ::to ::state)
(s/def ::ms number?)

(s/def ::when
  (s/or :spec qualified-keyword?
        :fn   fn?))

(s/def ::event-id
  (s/and keyword? (complement #{::timeout})))

(s/def ::event
  (s*/non-conformer
   (s/cat :event-id ::event-id
          :more     (s/* any?))))

(s/def ::timeout
  (s*/non-conformer
   (s/cat :id   #{::timeout}
          :ref  ::s*/ref
          :ms   ::ms
          :more (s/* any?))))

(s/def ::pull
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
           :opt-un [::when ::pull ::dispatch ::dispatch-later])
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

(s/def ::wildcard-transition
  (s/tuple #{:*} ::transition-to))

(s/def ::timeout-transition
  (s/tuple ::timeout ::transition-to))

(s/def ::transition
  (s*/conform-to
    (s/or :event    ::event-transition
          :wildcard ::wildcard-transition
          :timeout  ::timeout-transition)
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

(s/def ::max-one-wildcard-transition
  (fn [transitions]
    (->> transitions
         (filter (comp #{:wildcard} -type))
         (count)
         (>= 1))))

(defn- compile-transitions
  [transitions]
  (letfn [(f [t [k v]]
            (if (not= k :*)
              (t/add t k [k v])
              t))]
    (-> (group-by -type transitions)
        (update :timeout ffirst)
        (update :wildcard first)
        (assoc :event (reduce f (t/trie) transitions))
        (set/rename-keys
         {:event    :event-trie
          :wildcard :wildcard-transition}))))

(s/def ::transitions
  (s/and (s/conformer seq)
         (s/coll-of ::transition)
         ::max-one-timeout-transition
         ::max-one-wildcard-transition
         (s/conformer compile-transitions)))

(defn distribute-transitions
  [fsm]
  (letfn [(r2 [xtions]
            (fn [acc k m]
              (->> xtions
                   (merge m)
                   (assoc acc k))))

          (r1 [acc [t form] xtions]
            (case t
              :state (assoc acc form xtions)
              :fsm   (->> form
                          (reduce-kv (r2 xtions) {})
                          (merge acc))))]
    (reduce-kv r1 {} fsm)))

(s/def :parse-recur/fsm
  (s/and (s/map-of (s/or :state ::state :fsm :parse-recur/fsm)
                   (s/nilable map?)
                   :conform-keys true)
         (s/conformer distribute-transitions)))

(s/def :state-map/fsm
  (s/and :parse-recur/fsm
         (s/map-of ::state (s/nilable ::transitions))))

(s/def ::fsm-map
  (s/keys :req-un [::ref]
          :opt-un [:state-map/fsm
                   ::or ::stop
                   ::dispatch ::dispatch-later]))

(defn- default-nil-state
  "A `nil` state is a real state for an FSM, just like any
  other. However, it is also the default initial state for most
  FSMs. Every allowable state for an FSM must be fully enumerated in
  the :fsm spec, even if that state's transition map is nil. It is an
  error for the FSM to reach a state that is not enumerated in the
  transition map. Because the nil state is so common, it is always
  implicitly enumerated with a transition map of `nil`. However, this
  means an FSM can never get out of the nil state without an explicit
  {nil transition}, or the `:to` state being set."
  [expr]
  (update expr :fsm #(merge {nil nil} %)))

(s/def ::fsm
  (s/and (s/conformer default-nil-state) ::fsm-map))

(defn- parse
  [fsm]
  (-> (s*/parse ::fsm fsm)
      (assoc :fsm-unparsed fsm)))

(defn- eval-clause
  [db input {pull        :pull
             [type pred] :when}]
  (or (not pred)
      (let [f (case type
                :spec (partial s/valid? pred)
                :fn   pred)]
        (f (if pull
             (map #(db/getn db %) pull)
             input)))))

(defn- cond-clause
  [db [input clauses]]
  (some->> clauses
           (remove nil?)
           (filter (partial eval-clause db input))
           (first)))

(defn- match-transition
  "More specific events are always matched before wildcard."
  [db event {trie :event-trie
             wct  :wildcard-transition}]
  (or (t/match trie event) wct))

(defn- get-transition
  [{stop      :stop
    state-map :fsm
    unparsed  :fsm-unparsed} state]
  (if (or (contains? state-map state)
          (contains? stop state))
    (get state-map state)
    (-> "Not a valid FROM state"
        (ex-info {:state state :fsm unparsed})
        (throw))))

(defn- validate-to
  [{stop      :stop
    state-map :fsm
    unparsed  :fsm-unparsed} {to :to :as clause}]
  (when-not (or (contains? state-map to)
                (contains? stop to))
    (-> "Not a valid TO state"
        (ex-info {:state to :fsm unparsed})
        (throw)))
  clause)

(defn- match-clause
  "Given the current state of the FSM in the db, returns a matching
  clause. Care must be taken to handle the `nil` state."
  [fsm current-state db event]
  (some->> (get-transition fsm current-state)
           (match-transition db event)
           (cond-clause db)
           (validate-to fsm)))

(defn- get-fsm-fx
  "Given a parsed FSM, a timeout reference, a db, and an event, computes
  that FSM's advance fx, if any. After advance fx have been collected
  for all FSMs, they are all realized at the same time by
  `reflet.fsm/advance`, and their results merged. This effectively
  advances all FSMs at the same time, based on the same db value. Or,
  framed another way, makes FSM advance computations commutative. If
  we advance the FSM directly here, FSM logic is imperative, and not
  commutative. Do not write to unmounted transient entities."
  [{:keys [ref attr]
    :as   fsm} timeout db event]
  (when-not (db/transient-unmounted? ref)
    (let [state (db/get-inn db [ref attr])]
      (when-let [clause (match-clause fsm state db event)]
        {:fsm        fsm
         :clause     clause
         :timeout    timeout
         :prev-state state}))))

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
  [timeout fsm state]
  (when-let [[_ ref ms :as event-v] (get-timeout fsm state)]
    (when (= (:ref fsm) ref)
      (clear-timeout! timeout)
      (as-> #(f/dispatch-sync event-v) %
        (js/setTimeout % ms)
        (reset! timeout %)))))

(declare stop!)

(defn- fsm-dispatch!
  "Must happen after interceptor has been registered."
  [{:keys [dispatch dispatch-later]}]
  (doseq [event dispatch]
    (f/dispatch event))
  (doseq [event dispatch-later]
    (fx/dispatch-later event)))

(defn- do-fx!
  [{:keys [stop fsm-v]
    :as   fsm} clause timeout state]
  (fsm-dispatch! clause)
  (if (contains? stop state)
    (stop! fsm-v)
    (set-timeout! timeout fsm state)))

(defn- do-fx-after
  [event fsm-fx db]
  (reduce (fn [db {{:keys [ref attr]
                    :as   fsm}    :fsm
                   {:keys [to]
                    :as   clause} :clause
                   timeout        :timeout}]
            (do-fx! fsm clause timeout to)
            (db/assoc-inn db [ref attr] to))
          db
          fsm-fx))

(defn- trace-rf
  [t event]
  (fn [db {{ref   :ref
            fsm-v :fsm-v} :fsm
           :keys          [clause timeout prev-state]}]
    (->> {:t          t
          :fsm-v      fsm-v
          :event      event
          :clause     clause
          :prev-state prev-state}
         (update-in db
                    [::db/trace ::db/fsm->transition ref fsm-v]
                    util/qonj
                    (db/queue-size)))))

(defn- trace-after
  "Each FSM advance fx will increment the ::db/tick. But for debugging
  purposes, what we really want is the resultant ::db/tick after all
  the FSMs have been advanced. This unfortunately requires us to
  iterate twice through the fx set, though this is still fairly
  fast. When not debugging, this extra trace loop is bypassed."
  [event fsm-fx db]
  (if (db/trace? event)
    (-> (get-in db [::db/data ::db/tick])
        (trace-rf event)
        (reduce db fsm-fx))
    db))

(defn- advance-after
  "Collects FSM advance fx, and executes them."
  [{{db-fx :db}      :effects
    {db-cofx :db
     event   :event} :coeffects
    fsm-fx           ::fsm-fx
    :as              context}]
  (if (not-empty fsm-fx)
    (->> (or db-fx db-cofx)
         (do-fx-after event fsm-fx)
         (trace-after event fsm-fx)
         (assoc-in context [:effects :db]))
    context))

(def advance
  (f/->interceptor
   :id ::advance
   :after advance-after))

(def fsm-interceptors
  [db/inject-query-index
   db/trace-event
   advance
   (i/add-global-interceptors ::fsm)])

(i/reg-event-fx-impl ::timeout
  fsm-interceptors
  (constantly nil))

(defn- fsm-fx-interceptor-after
  [fx-fn]
  (fn [{{db-fx :db}      :effects
        {db-cofx :db
         event   :event} :coeffects
        :as              context}]
    (let [db (or db-fx db-cofx)
          fx (fx-fn db event)]
      (cond-> context
        fx (update ::fsm-fx util/conjs fx)))))

(defn- fsm-fx-interceptor
  [fx-fn]
  (f/->interceptor
   :id (gensym :_)
   :after (fsm-fx-interceptor-after fx-fn)))

(defn- fsm-spec
  [fsm-v]
  (let [[id & args] fsm-v]
    (or (some-> (reg/get-handler ::fsm-fn id)
                (apply args)
                (assoc :fsm-v fsm-v)
                (util/assoc-nil :attr ::state))
        (throw (ex-info "No FSM handler" {:fsm-v fsm-v})))))

(defn- first-trace!
  [{ref   :ref
    fsm-v :fsm-v} state t]
  (->> #queue [{:t          t
                :fsm-v      fsm-v
                :init-state state}]
       (swap! db/trace-index
              assoc-in
              [::db/fsm->transition ref fsm-v])))

(i/reg-event-db-impl ::advance
  fsm-interceptors
  (fn [db [_ fsm-v state]]
    (let [{:keys [ref attr]} (fsm-spec fsm-v)]
      (db/assoc-inn db [ref attr] state))))

(defn- init-state!
  [{:keys    [ref attr fsm-v]
    or-state :or} db]
  (let [db-state (db/get-inn db [ref attr])]
    (if (and or-state (nil? db-state))
      (do (f/dispatch [::advance fsm-v or-state])
          or-state)
      db-state)))

(defn- start-fx!
  [fsm timeout db]
  (let [state (init-state! fsm db)]
    (set-timeout! timeout fsm state)
    (when db/tap-fn
      (let [t (get-in db [::db/data ::db/tick])]
        (first-trace! fsm state t)))))

(defn running?
  [fsm-v]
  (i/global-interceptor-registered? ::fsm fsm-v))

(defn- start!
  "Do not use directly. Prefer ::start event or subscription."
  [db fsm-v]
  (let [fsm (parse (fsm-spec fsm-v))]
    (when-not (running? fsm-v)
      (let [timeout (atom nil)]
        (start-fx! fsm timeout db)
        (->> (partial get-fsm-fx fsm timeout)
             (fsm-fx-interceptor)
             (i/reg-global-interceptor ::fsm fsm-v))
        ;; Must happen after interceptor.
        (fsm-dispatch! fsm)))))

(defn- stop!
  "Do not use directly. Prefer ::stop event or subscription."
  [fsm-v]
  (when (i/same-cycle? fsm-v)
    (i/clear-global-interceptor ::fsm fsm-v)))

;; In general, an FSM cannot be reliably started or stopped via FX if
;; there is the possibility that a db mutation might occur at the same
;; time. This is due to the concurrency considerations of setting
;; timeouts. Whether or not there is an initial timeout depends on the
;; state of the FSM at the time the FSM is started. However, because
;; any interceptor can change the value of the db, and the interceptor
;; chain is dynamic, there is no reliable point in the interceptor
;; chain where we can take the value of the db to get the state of the
;; FSM. And because the order of FX is indeterminate, there is no way
;; to resolve the concurrency problem of when to measure the db value
;; in the FX itself. The only robust solution is to use
;; dedicated ::start and ::stop events where we can guarantee that
;; there is no db mutation happening.

(defn- lifecycle-fx
  [{{start-v ::start
     stop-v  ::stop} :effects
    :as              context}]
  (cond
    start-v (start! (.-state dbr/app-db) start-v)
    stop-v  (stop! stop-v))
  context)

(def lifecycle-interceptor
  (f/->interceptor
   :id ::lifecycle-interceptor
   :after lifecycle-fx))

(defn- register-lifecycle
  "Stopping and starting FSMs during the event phase is only safe if
  there are no concurrent mutations. Therefore we roll custom event
  handlers that process a single ::start or ::stop fx. We want to
  avoid exposing those fx via reg-fx as part of the user API."
  [id handler]
  (->> handler
       (fitor/fx-handler->interceptor)
       (vector lifecycle-interceptor)
       (events/register id)))

(register-lifecycle ::start
  (fn [_ [_ fsm-v]]
    {::start fsm-v}))

(register-lifecycle ::stop
  (fn [_ [_ fsm-v]]
    {::stop fsm-v}))

;;;; Subs

(defn- fsm-reaction
  "Instantiates the FSM, and returns a reaction to the FSM state. The
  FSM `start!` depends on the value of the db, but careful not to
  dereference it: reactivity must be via the pull-reaction."
  [db fsm-v]
  (let [{:keys [ref attr return]
         :or   {return attr}
         :as   fsm} (fsm-spec fsm-v)
        fsm-v*      (i/with-cycle-id fsm-v)
        start-fn    #(start! (.-state db) fsm-v*)
        stop-fn     #(stop! fsm-v*)
        return-fn   #(do [return ref])]
    (start-fn)
    (db/pull-reaction {:on-dispose stop-fn}
                      return-fn
                      fsm-v)))

(defn reg-fsm
  [id fsm-fn]
  (reg/register-handler ::fsm-fn id fsm-fn)
  (f/reg-sub-raw id fsm-reaction))
