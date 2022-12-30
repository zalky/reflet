(ns reflet.db
  "Provides an event sourced, reactive db, with mutation and query
  methods.

  Storing data in normalized form has many benefits for application
  and data design. The main trade-off is the performance cost of
  normalizing data during writes, and de-normalizing data during
  queries. Specifically, some normalized queries can end up spanning
  the entire db, and so in a reactive application, understanding when
  to re-run queries is key to performance.

  The naive approach of just re-running every query whenever the db
  changes is rarely feasible. Just one expensive query will grind the
  entire app to a halt, and predicting the cost of a query is a
  run-time calculation that depends on the data.

  A key insight that can help optimize query denormalization is that
  for the set of queries that conform to a Datomic pull, a result
  cannot change if the entities that were traversed while walking the
  joins have not changed.

  An approach used elsewhere that leverages this guarantee is to use
  reagent reactions to cache entity joins in the computed result. This
  avoids recomputing the entire query if some of the entities that
  were traversed in the joins have not changed. Only the subgraph that
  depends on the changed entities will be recomputed.

  Unfortunately this approach runs into a fundamental problem: by
  turning every join into a reactive computation, the cumulative
  overhead of tracking every reaction significantly exceeds the cost
  of just running the full non-reactive query. Equality checks are
  expensive, and the number of join reactions created depends on the
  shape of data at run-time, which can be huge. The naive approach, to
  just recompute the full query when the db changes, is always faster.

  The solution presented in this namespace leverages the guarantee on
  query results and combines it with an event sourcing strategy to
  implement a differential, reactive loop between the db and queries.

  To summarize: each query tracks the entities it traversed to produce
  its result. When a mutation operation is performed on the db, the
  entities that were touched by the mutation are then directly mapped
  to the queries that need to update. This is essentially a mutation
  event sourcing strategy.

  Importantly, while queries react to mutation events, they will also
  always return the correct value if for whatever reason the db is
  reset outside the mutation functions, for example, if the db value
  is set by Re-frame-undo or debug tooling like Re-frame-10x. The
  reactive queries respect time-travel.

  Another key design point is that query operations must be
  commutative and idempotent with respect to the tracking index. On
  the mutation side, any set of db operations that were previously
  commutative or idempotent on unnormalized data should remain so.

  The db mutation operations are pure functions that operate on the db
  value provided to event handlers. They can be arbitrarily composed
  in event handler code, similar to associative methods like
  `clojure.core/assoc`. They can also be freely interleaved with
  non-normalzied db operations. At the end of the day the Re-frame db
  is still just a big map.

  It is important to note that the query index is not actually stored
  as part of application state, since it is fundamentally not
  application state. Rather, it is state associated with the reactive
  queries, analogous to how reagent reactions maintain internal state
  to function properly. If you wind back application state to a
  previous value, the query state should not change until the queries
  have re-run.

  In order for the pure db mutation functions to operate on the index,
  an interceptor injects the index into the db coeffect, as well as
  removes and commits the updated index as part of the :after
  interceptor chain.

  Three iteration ticks drive the reactive algorithm:

  1. db tick
  2. index tick
  3. query tick one for each query

  Step by step:

  1. The db tick and index tick are incremented synchronously by
     the database mutation functions. If at any point the db tick
     and index tick do not match, this indicates time travel, and
     the query index is flushed by the mutation function.

  2. If an entity is touched by a db mutation function, then any
     query that was tracking that entity is marked as touched in
     the query index.

  3. During the subscription phase, a query must be re-run iff one or
     more of the following is true:

     a) the query is marked as touched in the query index,
     b) the query's tick is nil, indicating a fresh query index, or
     c) the db tick is not equal to the index tick, indicating
        time travel

  4. This conditional constitutes the ::query-tick signal. The signal
     returns the current query tick if the query does not need to
     re-run, or the current db tick if it does.

  5. When a query is re-run, it:
     a) updates the entities it is tracking in the query index
     b) clears the touched query set
     c) syncs the query tick to the db tick

  6. After all queries have been run, the index tick is synced to
     to the db tick.

  The above algorithm will only react to db changes via the db
  mutation functions. Specifically, the normalized data stored at
  the ::data key of the Re-frame db should never be directly
  manipulated, except via the mutation functions.

  However, aside from this one constraint, all regular db operation on
  un-normalized data are completely orthogonal to the differential
  reactive loop. At the end of the day, the db is still just a map,
  and you can mutate it in any way as long as you persist the ::data
  key.

  This namespace additionally provides an implementation for link
  queries similar to those in Fulcro or Om Next. A link attribute
  allows you to store normalized data at a semantically meaningful,
  global keyword in the normalized db. For example, you could store
  normalized user data at a global `::current-user` attribute in the
  db index.

  Finally, this algorithm is implemented entirely via existing reagent
  and Re-frame machinery."
  (:require [cinch.core :as util]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [re-frame.core :as f]
            [re-frame.db :as db]
            [re-frame.loggers :as log]
            [re-frame.registrar :as reg]
            [reagent.core :as r*]
            [reagent.ratom :as r]
            [reflet.config :as config]
            [reflet.db.normalize :as norm]
            [reflet.util.transients :as t])
  (:require-macros [reflet.db :refer [traced-reaction]]))

(defmulti random-ref*
  "Extend this to produce different kinds of random entity references
  for new id attributes."
  identity)

(defmethod random-ref* nil
  [_]
  (throw (js/Error "Ref must have unique id attribute")))

(defmethod random-ref* :default
  [id-attr]
  [id-attr (random-uuid)])

(defn ^:dynamic random-ref
  "Given a unique id attribute, and optionally metadata, returns a
  random entity reference. Only rebound for testing."
  [id-attr & [meta]]
  (cond-> (random-ref* id-attr)
    meta (update 1 with-meta meta)))

;;;; Transient Entity Reference Tracking

(extend-type UUID
  IWithMeta
  (-with-meta [o meta]
    (let [o* (uuid (.-uuid o))]
      (set! (.-metadata o*) meta)
      o*))

  IMeta
  (-meta [o]
    (.-metadata o)))

(extend-type Keyword
  IWithMeta
  (-with-meta [o meta]
    (let [o* (Keyword. (.-ns o)
                       (.-name o)
                       (.-fqn o)
                       (.-_hash o))]
      (set! (.-metadata o*) meta)
      o*))

  IMeta
  (-meta [o]
    (.-metadata o)))

(defn ref-meta
  "Returns metadata for entity reference"
  [ref]
  (when (sequential? ref)
    (meta (second ref))))

(defn transient?
  "Returns true if the given entity references is transient."
  [ref]
  (boolean
   (some-> ref ref-meta :transient)))

(defonce mounted-transient-refs
  ;; The set of transient entity references that are associated with
  ;; mounted components. These are not stored in app state since this
  ;; is implementation state to do with the lifecycle of components.
  (r/atom #{}))

(defn mount-ref!
  [ref]
  (swap! mounted-transient-refs conj ref))

(defn unmount-ref!
  [ref]
  (swap! mounted-transient-refs disj ref))

(f/reg-fx ::unmount-ref
  unmount-ref!)

(defn mounted?
  "Returns true if the given entity references is associated with a
  component that is currently mounted. This is useful for long running
  tasks that should short circuit if the associated component has been
  unmounted."
  [ref]
  (contains? @mounted-transient-refs ref))

(defn transient-unmounted?
  "Returns true if the given reference is unmounted and transient."
  [ref]
  (and (transient? ref)
       (not (mounted? ref))))

;;;; DB Xtors

(def default-unique-attributes
  #{:system/uuid
    :cmp/uuid
    :el/uuid
    :js/uuid
    :debug/id})

(f/reg-sub ::id-attrs
  (fn [db _]
    (get db ::id-attrs)))

(defn- valid-db?
  [data id-attrs]
  (and (or (nil? id-attrs)
           (set? id-attrs))
       (map? data)))

(defn new-db
  "Returns a new db, optionally with initial data and unique id
  attributes. Must have at least one unique id attribute."
  ([]
   (new-db {}))
  ([data]
   (new-db data nil))
  ([data id-attrs]
   {:pre [(valid-db? data id-attrs)]}
   (->> (or id-attrs default-unique-attributes)
        (assoc data ::id-attrs))))

;;;; Query Tracking Index

(defonce query-index
  ;; Query index for tracking touched entities. Note that this is NOT
  ;; application state. Rather, it is state associated with the
  ;; differential, reactive queries, analogous to how reagent
  ;; reactions maintain internal state to function properly.
  (r/atom {}))

(defn- q->e
  [index q-ref & [or]]
  (get-in index [::q->e q-ref] or))

(defn- e->q
  [index e-ref & [or]]
  (get-in index [::e->q e-ref] or))

(defn- q->tick
  [index q-ref & [or]]
  (get-in index [::q->tick q-ref] or))

;;;; Trace

(defonce tap-fn
  nil)

(defonce trace-index
  (r/atom {}))

(defn queue-size
  "Number of events to tap per query. This should eventually be
  dynamic."
  []
  (-> (config/get-config)
      (get :trace-queue-size 50)))

(defn- trace-v?
  [v]
  (let [id (first v)]
    (or (= id ::untrace-event)
        (let [ns (namespace id)]
          (not (str/starts-with? ns "reflet.debug"))))))

(defn trace?
  [v]
  (and tap-fn (trace-v? v)))

(defn- trace-event-before
  [{{event :event} :coeffects
    :as            context}]
  (if (trace? event)
    (assoc-in context
              [:coeffects :db ::trace]
              @trace-index)
    context))

(defn- event-refs
  "Includes both refs touched by event db mutations, as well as
  positional parameters in the event vector."
  [index [id :as event] id-attrs]
  (when-not (= id ::untrace-event)
    (set/union
     (some->> (rest event)
              (filter #(norm/ref? % id-attrs))
              (not-empty)
              (set))
     (::touched-entities index))))

(defn- commit-trace!
  [{{t   ::tick
     :as index} ::index
    trace       ::trace
    id-attrs    ::id-attrs
    :as         db} event]
  (letfn [(f [m refs]
            (reduce rf m refs))

          (rf [m q]
            (->> {:t t :event event}
                 (update m q util/qonj (queue-size))))]
    (when trace
      (->> (event-refs index event id-attrs)
           (update trace ::e->event f)
           (reset! trace-index)))))

(defn- trace-event-after
  [{{event :event} :coeffects
    {db :db}       :effects
    :as            context}]
  (if (and db (trace? event))
    (do (commit-trace! db event)
        (update-in context [:effects :db] dissoc ::trace))
    context))

(def trace-event
  (f/->interceptor
   :id ::trace-event
   :before trace-event-before
   :after trace-event-after))

(f/reg-event-db ::untrace-event
  ;; Only debug event specifically enabled in `trace-v?`
  [trace-event]
  (fn [db [_ ref]]
    (-> db
        (update-in [::trace ::e->event] dissoc ref)
        (update-in [::trace ::fsm->transition] dissoc ref))))

(defn- trace-query
  [q-ref query-v q-tick result]
  (when (trace? query-v)
    (->> {:result  result
          :query-v query-v
          :t       q-tick}
         (swap! query-index
                update-in
                [::q->trace q-ref query-v]
                util/qonj
                (queue-size))))
  result)

(defn- untrace-query
  [q-ref query-v]
  (when (trace? query-v)
    (swap! query-index update ::q->trace dissoc q-ref)))

;;;; Write API

(defn- touch-queries*
  [u-fn index e-ref]
  (if-let [queries (e->q index e-ref)]
    (-> index
        (u-fn ::touched-queries set/union queries)
        (u-fn ::touched-entities util/conjs e-ref))
    index))

(defn- touch-queries
  "Given a normalized entity ref, touches any queries that are tracking
  that entity in the index."
  [index e-ref]
  (touch-queries* update index e-ref))

(defn- touch-queries!
  "Transient version of touch-queries."
  [index e-ref]
  (touch-queries* t/update! index e-ref))

(defn- flush-index
  [db]
  (assoc db ::index {}))

(defn- inc-tick
  "Given a db with an injected query index, increments the db tick and
  syncs it to the index tick. If the two ticks were initially out of
  sync, this indicates time travel, and the query index is flushed."
  [db]
  (let [index-tick (get-in db [::index ::tick])
        db-tick    (get-in db [::data ::tick])
        flush?     (not= db-tick index-tick)
        new-tick   (inc db-tick)]
    (-> db
        (cond-> flush? flush-index)
        (assoc-in [::index ::tick] new-tick)
        (assoc-in [::data ::tick] new-tick))))

(defn- warn-on-transient-write
  [ref]
  (when (transient-unmounted? ref)
    (log/console :warn "Writing to unmounted transient state" ref)))

(defn- mergen-normalize
  [tx {:keys [id-attrs] :as opts}]
  (->> (norm/to-many tx id-attrs)
       (filter map?)
       (mapcat #(norm/normalize % opts))))

(defn- get-opts
  [db]
  {:id-attrs (::id-attrs db)})

(defn mergen
  "Normalizes the given tx data, merges all normalized entities into the
  db, touches any queries tracking those entities in the index, and
  increments the db and index ticks."
  [db tx]
  (let [opts (get-opts db)]
    (loop [data       (transient (::data db {}))
           index      (transient (::index db {}))
           [e & more] (mergen-normalize tx opts)]
      (if e
        (let [ref (norm/refer-one e opts)]
          (warn-on-transient-write ref)
          (recur (t/update! data ref merge e)
                 (touch-queries! index ref)
                 more))
        (-> db
            (assoc ::data (persistent! data))
            (assoc ::index (persistent! index))
            (inc-tick))))))

(defn- valid-path?
  [db path]
  (and (->> db
            (::id-attrs)
            (norm/ref? (first path)))
       (<= (count path) 2)))

(defn assoc-inn
  "Adds the unnormalized value to entity at path. Similar semantics to
  assoc-in, where the entity ref is first part of path. Additionally
  touches any queries that are tracking the entity and increments the
  db and index ticks. Does not resolve entity references, so it cannot
  do deep updates in normalized data."
  [db [ref :as path] value]
  {:pre [(valid-path? db path)]}
  (warn-on-transient-write ref)
  (let [p (cons ::data path)]
    (-> db
        (assoc-in p value)
        (update ::index touch-queries ref)
        (inc-tick))))

(defn update-inn
  "Updates the value at the normalized db path. Similar semantics to
  clojure.core/update-in, where the entity ref or link attribute is
  first part of path. The updated value is either an entity, a link
  attribute value, or an attribute of an entity. Does not resolve
  entity references, so it cannot do deep updates in normalized data."
  [db [ref :as path] f & args]
  {:pre [(valid-path? db path)]}
  (warn-on-transient-write ref)
  (let [p (cons ::data path)]
    (-> (apply update-in db p f args)
        (update ::index touch-queries ref)
        (inc-tick))))

(defn assocn
  "Adds a normalized link at the given attribute."
  [db attr tx]
  {:pre [(identity attr)]}
  (letfn [(assocn* [refs]
            (-> db
                (mergen tx)
                (assoc-in [::data attr] refs)
                (update ::index touch-queries attr)))]
    (let [opts (get-opts db)]
      (if-let [ref (norm/refer-one tx opts)]
        (assocn* ref)
        (if-let [refs (norm/refer-many tx opts)]
          (assocn* (norm/like tx refs))
          db)))))

(defn valid-updaten?
  [{::keys [id-attrs]} ref tx]
  (or (norm/ref? ref id-attrs)
      (and (keyword? ref)
           (every? #(norm/ref? % id-attrs) tx))))

(defn updaten
  "Updates a normalized link at the given attribute."
  [db ref f tx]
  {:pre [(valid-updaten? db ref tx)]}
  (-> db
      (update-in [::data ref] f tx)
      (update ::index touch-queries ref)
      (inc-tick)))

(defn- normalize-and-filter
  [tx data opts]
  (->> tx
       (map #(or (norm/refer-one % opts) %))
       (filter #(contains? data %))))

(defn dissocn
  "Removes the list of entities, refs, or links from the db, touches any
  queries tracking those entities in the index, and increments the db
  and index ticks. Does not remove any nested entities in tx."
  [db & tx]
  (let [opts (get-opts db)]
    (loop [data         (transient (::data db {}))
           index        (transient (::index db {}))
           [ref & more] (normalize-and-filter tx data opts)]
      (if ref
        (recur (dissoc! data ref)
               (touch-queries! index ref)
               more)
        (-> db
            (assoc ::data (persistent! data))
            (assoc ::index (persistent! index))
            (inc-tick))))))

;;;; Query API

(declare pull*)

(defn prop?
  [expr]
  (or (keyword? expr) (string? expr)))

(defn- link?
  [ref expr]
  (and (map? expr) (not ref)))

(defn- wildcard?
  [expr]
  (= expr '*))

(defn- pull-props
  [{:keys [db ref acc-fn] :as context} pattern result]
  (let [c (assoc context
                 :entity (get db ref)
                 :pattern pattern)]
    (when (and acc-fn ref)
      (acc-fn ref))
    (reduce (fn [result expr]
              (pull* c expr result))
            result
            (distinct pattern))))

(defn- pull-prop
  [{:keys [entity]} attr result]
  (let [v (get entity attr)]
    (if (some? v)
      (assoc result attr v)
      result)))

(defn- unquote-list
  [effect]
  (case (first effect)
    list (rest effect)
    effect))

(defn- pull-effects
  "Parses effect expression and runs effect-fn, if configured, for side
  effects. The pull-fx-fn has two args: a map of effect parameters,
  and a subset of the query context. The default pull implementation
  should have no effects configured, and is pure."
  [{:keys [db ref pull-fx-fn]
    :as   context} effect result]
  (let [[expr params] (unquote-list effect)]
    (when pull-fx-fn
      (pull-fx-fn params {:db db :ref ref}))
    (pull* context expr result)))

(defn- pull-join
  [{:keys [entity pattern id-attrs join?] :as context} join result]
  (letfn [(dec-recursive-join []
            (mapv
             (fn [expr]
               (if (= expr join)
                 (let [[[k n]] (seq expr)]
                   {k (dec n)})
                 expr))
             pattern))

          (recursive-join-expr []
            (let [[[attr expr]] (seq join)]
              (if (number? expr)
                [attr (when (pos? expr)
                        (dec-recursive-join))]
                (if (= expr '...)
                  [attr pattern]
                  [attr expr]))))

          (r-pull [expr value]
            (if (and (norm/ref? value id-attrs)
                     (or (not join?)
                         (join? value)))
              (-> context
                  (assoc :ref value)
                  (pull* expr))
              value))

          (walk-joins [expr value]
            ;; Must not be lazy
            (if expr
              (if (norm/many? value id-attrs)
                (cond->> (doall (map #(r-pull expr %) value))
                  (not (seq? value)) (into (empty value)))
                (r-pull expr value))
              value))

          (walk-attr [result attr expr]
            (if-let [value (some->> attr (get entity))]
              (->> value
                   (walk-joins expr)
                   (assoc result attr))
              result))]

    (let [[attr expr] (recursive-join-expr)]
      (if (wildcard? attr)
        (let [attrs (keys entity)]
          (->> (repeat expr)
               (zipmap attrs)
               (reduce-kv walk-attr result)))
        (walk-attr result attr expr)))))

(defn- pull-link
  [{:keys [db acc-fn] :as context} link result]
  (let [[[attr]] (seq link)]
    (when (and acc-fn attr)
      (acc-fn attr))
    (-> context
        (assoc :entity db)
        (pull-join link result)
        (get attr))))

(defn- pull-wildcard
  [{:keys [entity]} _ result]
  (merge entity result))

(defn- pull*
  ([context expr]
   (pull* context expr nil))
  ([{ref :ref :as context} expr result]
   (cond
     (link? ref expr) (pull-link context expr result)
     (list? expr)     (pull-effects context expr result)
     (vector? expr)   (pull-props context expr result)
     (prop? expr)     (pull-prop context expr result)
     (wildcard? expr) (pull-wildcard context expr result)
     (map? expr)      (pull-join context expr result)
     :else            (throw (ex-info "Invalid pull expression"
                                      {::expr expr})))))

(defn- attr-expr
  [expr]
  (cond
    (keyword? expr) expr
    (map? expr)     (ffirst expr)
    (list? expr)    (attr-expr (first expr))
    :else           nil))

(defn- default-pull-impl
  "Evaluates the pull expression, `expr`, against the `:db` given in the
  `context` map. `context` will contain at least:

  :db
            DB value against which the expression is evaluated

  :id-attrs
            A set of unique id attributes that represent joins
            to other entities.

  And optionally:

  :ref
            Root entity reference with which to start the graph
            traversal. If it is omitted, the query is considered
            to be a link query. [Optional]

  :acc-fn
            Fn that accumulates entity references via side
            effects. [Optional]

  :pull-fx-fn
            Fn that handles pull side effects, like triggering
            data syncs. [Optional]

  After evaluating the expression against the value of the db, this
  function returns the result.
  
  By default, this implementation is a pure function of the given
  `:db` value. However, if `:acc-fn` and `:pull-fx-fn` are provided in
  the context map, this implementation runs them for
  side-effects. This means the entire impl must also be eager. This
  function is not meant to be called directly."
  [{ref :ref :as context} expr]
  (if-let [attr (when ref (attr-expr expr))]
    (-> (pull* context [expr])
        (get attr))
    (pull* context expr)))

(defn get-pull-fn
  "Retrieves configured pull fn, or default implementation."
  []
  (-> (config/get-config)
      (get :pull-fn default-pull-impl)))

(defn getn
  "Pulls the normalized entity from the db at the given path. Uses get
  semantics. This is a non-reactive fn meant to be used in event
  handlers."
  [db ref & [or]]
  (get-in db [::data ref] or))

(defn get-inn
  "Pulls normalized data from the given db at the given path. First
  element in path should be an entity ref. Uses get-in semantics. Does
  not resolve references. This is a non-reactive fn meant to be used
  in event handlers."
  [db [ref :as path] & [or]]
  (get-in db (cons ::data path) or))

(defn pull
  "Pulls normalized data from the given db. This is a non-reactive,
  functionally pure version of pull-reactive for use in event
  handlers. The data is pulled from the entity according to a pull
  pattern that conforms to:

  { :attr sub-pattern }
            If the entity contains :attr, it is treated as an
            entity reference and resolved. The algorithm then pulls
            the sub-pattern from the resolved entity.

  [ sub-pattern-1 sub-pattern-2 sub-pattern-3 ]
            The algorithm attempts to pull each sub-pattern from the
            given entity, and merging all the results together.

  keyword
            If the entity contains the keyword, includes it in the
            entity result.
  
  '*
            (literal symbol asterisk) Includes all attributes from
            the entity in the result.

  { :attr '... }
            Similar to the map pattern, but is recursive where the
            sub-pattern is the same as the parent entity reference
            and resolved. The algorithm then pulls the sub-pattern
            from the resolved entity. The ellipses can be replaced
            with an natural number to specify a limit to the
            recursion."
  ([db expr]
   (pull db expr nil))
  ([db expr e-ref]
   (let [pfn (get-pull-fn)]
     (pfn {:id-attrs (::id-attrs db)
           :db       (::data db)
           :ref      e-ref}
          expr))))

(defn- clear-stale-entities
  [index stale-entities q-ref]
  (persistent!
   (reduce (fn [index e-ref]
             (if-let [queries (get index e-ref)]
               (let [queries* (disj queries q-ref)]
                 (if (empty? queries*)
                   (dissoc! index e-ref)
                   (assoc! index e-ref queries*)))
               index))
           (transient (or index {}))
           stale-entities)))

(defn- add-fresh-entities
  [index fresh-entities q-ref]
  (persistent!
   (reduce (fn [index e-ref]
             (t/update! index e-ref util/conjs q-ref))
           (transient (or index {}))
           fresh-entities)))

(defn- update-index
  [index fresh stale query-tick q-ref]
  (-> index
      (assoc-in [::q->e q-ref] @fresh)
      (update ::e->q clear-stale-entities @stale q-ref)
      (update ::e->q add-fresh-entities @fresh q-ref)
      (assoc-in [::q->tick q-ref] query-tick)
      (assoc ::touched-queries #{})
      (assoc ::touched-entities #{})))

(defn- acc-fn
  "Accumulates freshly touched entity refs in volatiles."
  [fresh stale]
  (fn [e-ref]
    (vswap! fresh conj e-ref)
    (vswap! stale disj e-ref)))

(defmulti pull-fx
  (fn [params context]
    (:id params)))

(defn- pull-reactive
  "Given a pull expression and an entity reference, returns the
  de-normalized entity data from the database. Additionally adds newly
  tracked entities to the query index, updates the given query tick
  for the given query, clears stale entities from the previous pull,
  and clears touched queries."
  [{:keys [db index expr e-ref q-ref query-tick]}]
  (let [fresh  (volatile! #{})
        stale  (volatile! (q->e index q-ref #{}))
        pfn    (get-pull-fn)
        result (pfn {:id-attrs   (::id-attrs db)
                     :db         (::data db)
                     :ref        e-ref
                     :acc-fn     (acc-fn fresh stale)
                     :pull-fx-fn pull-fx}
                    expr)]
    {:db     db
     :result result
     :index  (update-index index fresh stale query-tick q-ref)}))

(defn- dispose-query
  [index q-ref]
  (let [stale (q->e index q-ref)]
    (-> index
        (update ::e->q clear-stale-entities stale q-ref)
        (update ::q->e dissoc q-ref)
        (update ::q->tick dissoc q-ref))))

;;;; Query index injection interceptor

(defn- inject-index-before
  [context]
  (assoc-in context [:coeffects :db ::index] @query-index))

(defn- inject-index-after
  [context]
  (if-let [index (get-in context [:effects :db ::index])]
    (-> context
        (update-in [:effects :db] dissoc ::index)
        (assoc-in [:effects ::index] index))
    context))

(def inject-query-index
  "Injects query index into app state for normalized data fns to work."
  (f/->interceptor
   :id ::inject-index
   :before inject-index-before
   :after inject-index-after))

(f/reg-fx ::index
  (fn [index]
    (reset! query-index index)))

;;;; Subs

(f/reg-sub ::db-tick
  (fn [{{db-tick ::tick} ::data} _]
    (r*/after-render
     (fn []
       (swap! query-index assoc ::tick db-tick)))
    db-tick))

(defn query-tick
  "Returns the given query's tick, syncing to the db tick when the query
  must re-run. The query must re-run whenever it has been touched, it
  is uninitialized, or the db tick and index ticks are out of sync,
  indicating time-travel. We do not cache or trace these reactions, as
  they are unique to each pull query, and considered an implementation
  detail."
  [q-ref]
  (let [db-tick (f/subscribe [::db-tick])]
    (r/reaction
      (let [index      (.-state query-index)
            touched    (::touched-queries index)
            index-tick (::tick index)
            query-tick (q->tick index q-ref)]
        (if (or (contains? touched q-ref)
                (nil? query-tick)
                (not= @db-tick index-tick))
          @db-tick                      ; Next query tick = db tick
          query-tick)))))

(defn query-ref
  []
  (random-ref :query/uuid))

(defn reactive?
  [x]
  (satisfies? r/IReactiveAtom x))

(defn maybe-deref
  [^js x]
  (cond-> x
    (reactive? x) deref))

(defn get-result-v
  [[id & args]]
  (let [ns (namespace id)
        n  (str (name id) "[result-fn]")]
    (-> (keyword ns n)
        (cons args)
        (vec))))

(defn reaction-ref
  [^clj input-r]
  (.-reflet-query-ref input-r))

(defn result-reaction
  [input-r result-fn query-v]
  (let [result-v  (get-result-v query-v)
        input-ref (reaction-ref input-r)]
    (traced-reaction input-ref result-v
      (fn []
        (let [t (-> (.-state db/app-db) ::data ::tick)]
          (->> (rest result-v)
               (map maybe-deref)
               (apply result-fn @input-r)
               (trace-query input-ref result-v t))))
      (fn []
        (untrace-query input-ref result-v)))))

(defn pull-reaction
  "Returns a differential pull reaction. `expr-fn` is a function that
  given query vector arguments, returns a pull query spec and
  optionally an entity ref that is being queried. `query-v` is the
  query vector supplied to the generating subscription. The tracing of
  this reaction was patterned after the `re-frame.sub/reg-sub`
  implementation, and allows subscriptions based on these reactions to
  be inspected in `re-frame-10x`. Note that while the computation in
  the pull reaction is dependent on the db and query index values, it
  is not reactive to them. Instead, it is reactive to changes the
  query tick, which tracks the db-tick and synced."
  [config expr-fn query-v]
  (let [q-ref  (query-ref)
        q-tick (query-tick q-ref)
        expr-r (apply expr-fn (rest query-v))]
    (traced-reaction q-ref query-v
      (fn []
        (let [in {:db         (.-state db/app-db)
                  :index      (.-state query-index)
                  :expr       (first expr-r)
                  :e-ref      (second expr-r)
                  :q-ref      q-ref
                  :query-tick @q-tick}

              {i :index
               r :result} (pull-reactive (merge config in))]
          (reset! query-index i)
          (trace-query q-ref query-v (.-state q-tick) r)))
      (fn []
        (swap! query-index dispose-query q-ref)
        (untrace-query q-ref query-v)
        (when-let [f (:on-dispose config)]
          (f))))))

;;;; Utils

(defn get-label
  "Returns any metadata label associated with the given entity
  reference."
  [ref]
  (some-> ref ref-meta :label))

(defn db-label-filter
  "Returns a version of the Re-frame db filtered by label."
  [& [label]]
  (letfn [(f [[ref _]]
            (when (vector? ref)
              (cond-> (get-label ref)
                label (= label))))]
    (->> @db/app-db
         (::data)
         (filter f)
         (into {}))))

(defn add-label
  [tx label]
  (walk/postwalk
   (fn [x]
     (if (uuid? x)
       (vary-meta x assoc :label label)
       x))
   tx))
