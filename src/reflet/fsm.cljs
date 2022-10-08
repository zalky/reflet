(ns reflet.fsm
  "Provides finite state machine DSL and implementation.

  This fsm implementation is based on an entity model, where any
  entity in the db can be transitioned through allowed states given
  defined inputs. This means both domain entities as well as component
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

  Here, the `:fsm` attribute defines a state map, mapping each fsm
  state to allowed transitions for those states.

  Each allowed transition is a map between an input, and one or more
  output clauses.

  There three types of transitions currently implemented, each
  corresponding to their input type:

  1. Event transitions
  2. Entity transitions
  3. Timeout transitions

  Only one entity or timeout transition is allowed in a state's
  transition map. However, a state's transition map can have an
  arbitrary number of event transitions. If a state defines both an
  entity and event transitions, the event transitions will always be
  matched before the entity transition.

  Recieved events are matched to an event input stem. The longest
  event stem in the transition map is matched first. This means for
  the recieved event `[:voted self first-pref second-pref]`, and the
  set of input keys:

  [:voted self]
  [:voted self first-pref]

  The matching key would be:
  
  [:voted self first-pref]

  Entity inputs are expressed as a vector of entity references, each
  reference being a tuple of unique attribute and a uuid.

  Timeout inputs are just normal events vector where the first three
  positional elements are: `[::timeout ref ms ...]`, where `ref` is an
  entity reference, and `ms` is the timeout duration in milliseconds.

  If the FSM implementation parses a timeout input whose `ref` is the
  same as the FSM `id` it will ensure that those timeouts will fire
  for their designated state, and be cleaned up appropriately.

  All transition inputs match one or more output clauses. Each output
  clause be either simple or expanded form.

  1. Simple: Just a state keyword
  2. Complex: A map containing the following attributes:

  `:to`         - The next state to transition to [required]
  `:when`       - The id of a Clojure spec that must return s/valid?
                  true for the transition input in order for the
                  transition to fire. For event inputs, this is
                  simply the full recieved event vector. For entity
                  inputs, the entity references are pulled from the
                  db, and passed to the Clojure spec. [optional]
  `:dispatch`   - An event vector to dispatch on a succesful
                  transition [optional]
  
  Other root FSM attributes:

  `:id`         - A  db reference to the fsm entity being advanced
                  through states [required]

  `:start`      - The default starting state if the entity is not
                  already in a state [optional]

  `:attr`       - The entity attribute where the state is
                  stored, when not provided `::state` is used
                  [optional]
  
  `:stop`       - The state which when reach will stop the fsm
                  [optional]

  `:return`     - A pull spec run as the return value of the
                  resultant FSM subscription. By default, the fsm
                  subscription returns a simple attribute query on
                  the state attribute specified by `:attr`. The
                  query is always run agains the FSM `:id` as the
                  root reference. [optional]

  An fsm can be started and stopped by dispatching the `::start` and
  `::stop` events, respectively.

  Because the FSM implementation is based on global interceptors that
  run every time, all the matching and lookup algorithms are written
  to be very fast."
  (:require [clojure.spec.alpha :as s]
            [re-frame.core :as f]
            [re-frame.registrar :as reg]
            [reagent.ratom :as r]
            [reflet.db :as db]
            [reflet.interceptors :as i]
            [reflet.trie :as t]
            [reflet.util.spec :as s*]))

(s/def ::id ::s*/ref)
(s/def ::state qualified-keyword?)
(s/def ::when qualified-keyword?)
(s/def ::to ::state)
(s/def ::start ::state)

(s/def ::event-id
  (s/and keyword? (complement #{::timeout})))

(s/def ::event
  (s*/identity-conformer
   (s/cat :event-id   ::event-id
          :more       (s/* any?))))

(s/def ::timeout
  (s*/identity-conformer
   (s/cat :id     #{::timeout}
          :ref    ::s*/ref
          :number number?
          :more   (s/* any?))))

(s/def ::entity-vec
  (s/coll-of ::s*/ref :kind vector?))

(s/def ::dispatch
  (s*/any-cardinality ::event :coerce-many true))

(s/def ::stop
  (s/coll-of ::state :kind set?))

(s/def ::transition-to-expanded
  (s/keys :req-un [::to]
          :opt-un [::dispatch ::when]))

(s/def ::transition-to-simple keyword?)

(s/def ::transition-to
  (s*/conform-to
    (s/or :simple   ::transition-to-simple
          :expanded ::transition-to-expanded)
    (fn [[t form]]
      (case t
        :simple   {:to form}
        :expanded form))))

(s/def ::transition-to-any
  (s*/any-cardinality ::transition-to :coerce-many true))

(s/def ::event-transition
  (s/tuple ::event ::transition-to-any))

(s/def ::entity-transition
  (s/tuple (s/coll-of ::s*/ref) ::transition-to-any))

(s/def ::timeout-transition
  (s/tuple ::timeout ::transition-to-any))

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
        (dissoc :event)
        (assoc :event-trie
               (reduce f (t/trie) transitions)))))

(s/def ::transitions
  (s/and (s/conformer seq)
         (s/coll-of ::transition)
         ::max-one-timeout-transition
         ::max-one-entity-transition
         (s/conformer compile-transitions)))

(s/def :state-map/fsm
  (s/map-of ::state ::transitions))

(s/def ::fsm
  (s/keys :req-un [::id ::start :state-map/fsm]
          :opt-un [::stop ::dispatch]))

(defn- parse
  [fsm]
  (s*/assert! ::fsm fsm))

(defn- cond-clause
  [[input clauses]]
  (->> clauses
       (remove nil?)
       (filter (fn [{c :when}] (or (not c) (s/valid? c input))))
       (first)))

(defn- get-transition
  [db event {trie   :event-trie
             entity :entity}]
  (or (t/match trie event)
      (when entity
        (-> (partial db/getn db)
            (map (first entity))
            (vector (second entity))))))

(defn- transition!
  [db event transition]
  (when-let [clause (->> transition
                         (get-transition db event)
                         (cond-clause))]
    (let [{:keys [to dispatch]} clause]
      (when dispatch
        (doseq [e dispatch]
          (f/dispatch e)))
      to)))

(defn- next-state
  "Returns next state if there is a valid transition, `nil` otherwise."
  [fsm db event]
  (let [{state-map :fsm
         :keys     [id attr start]
         :or       {attr ::state}} fsm]
    (->> (db/get-inn db [id attr] start)
         (get state-map)
         (transition! db event))))

(defn- get-timeout
  [fsm state]
  (some-> fsm
          (:fsm)
          (get state)
          (:timeout)))

(defn- self-timeout?
  [fsm ref]
  
  (throw
   (ex-info
    "Only self timeouts allowed in FSMs"
    {:fsm fsm})))

(defn- clear-timeout!
  [timeout]
  (some-> @timeout (js/clearTimeout)))

(defn- set-timeout!
  [fsm timeout state]
  (when-let [[_ ref ms
              :as event-v] (get-timeout fsm state)]
    (when (= (:id fsm) ref)
      (clear-timeout! timeout)
      (as-> #(f/dispatch event-v) %
        (js/setTimeout % ms)
        (reset! timeout %)))))

(declare stop!)

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
            (stop! fsm-v))
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
