(ns reflet.db
  "Provides db, normalized query and mutation methods, and differential,
  reactive pull subscriptions.

  Storing data in normalized form has many benefits for application
  architecture as well as data integrity. The main trade-off is the
  performance cost of normalizing data during writes, and
  de-normalizing data during queries.

  A key insight that can help optimize denormalization is that for the
  set of queries that conform to a Datomic pull, a result cannot
  change if the entities that were looked up to produce the result
  have not changed. The exception to this are Datomic reverse
  attribute lookups, which can be avoided.

  The Subgraph library, for example, leverages this guarantee to cache
  entity lookups via reactions in the computed result. This avoids
  recomputing the result if none of the entities that were looked up
  have changed.
  
  The problem with this approach is that all those lookup reactions
  depend directly on the db itself, and the cumulative overhead of
  managing those reactions often significantly outweighs the marginal
  benefit of not having to recompute the query result every time. The
  naive method of just recomputing the query result every time is
  actually faster.

  Unfortunately, the naive method is not good enough:

  With normalized data, any query may potentially span the entire db,
  and therefore every query must re-run whenever the db changes. While
  this may work if every query is cheap, just one expensive query will
  grind the entire app to halt.

  The solution presented in this namespace leverages the guarantee on
  query results to implement a differential, reactive loop between the
  db and queries.

  The key component is a symmetric index for mapping queries to
  entities and entities to queries. When a query runs, it updates the
  index with entities it is tracking. On the write side, when a db
  mutation function touches a normalized entity, it can trivially
  lookup in the index which queries must re-run. Those queries that do
  not need to re-run can return a cached result.
  
  Any set of query operations are commutative and idempotent. This is
  true even though the query reactions update the query index via
  side-effect.

  Any set of db mutation operations that were previously commutative
  or idempotent on unnormalized data should remain so. The db mutation
  functions are pure and operate on the db value provided to event
  handlers. They can be arbitrarily composed in event handler code,
  similar to associative methods like `clojure.core/assoc`.

  It is important to note that the query index is not actually stored
  as part of application state, since it is inherently not application
  state. Rather, it is state associated with the reactive queries,
  analogous to how reagent reactions maintain internal state to
  function properly. If you wind back application state to a previous
  value, the query state should not change until the queries have
  re-run.

  In order for the pure db mutation functions to operate on the index,
  an interceptor injects the index into the db coeffect, as well as
  removes and commits the updated index as part of the :after
  interceptor chain.

  Three iteration ticks drive the reactive algorithm:

  1. db tick
  2. index tick
  3. query tick (one for each query)

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
  the ::data key of the re-frame db should never be directly
  manipulated, except via the mutation functions.

  However, aside from this one constraint, all regular db operation on
  un-normalized data are completely orthogonal to the differential
  reactive loop. At the end of the day, the db is still just a map.
  While it is probably not recommended for broader architectural
  reasons, it is technically feasible to interleave normalized db
  operation with un-normalized ones in the same event handler.

  This namespace additionally provides an implementation for link
  queries similar to those in Fulcro or Om Next. A link attribute
  allows you to store normalized data at a semantincally meaningful,
  global keyword in the normalized db. For example, you could store
  normalized user data at a global `:current-user` attribute in the db
  index. A query on a link attribute poses an edge case when it
  updates: a link query must not only track the entities used to
  construct the result, but the value of the link attribute itself.

  Finally, this algorithm is implemented entirely via the existing
  reagent and re-frame machinery."
  (:require [clojure.set :as set]
            [clojure.walk :as walk]
            [com.rpl.specter :as sp]
            [re-frame.core :as f]
            [re-frame.db :as db]
            [re-frame.interop :as interop]
            [re-frame.trace :as trace]
            [reagent.core :as r*]
            [reagent.ratom :as r]
            [reflet.db.normalize :as norm]
            [reflet.util :as util]
            [reflet.util.transients :as t]
            [taoensso.timbre :as log]))

(defmulti random-ref*
  "Given a unique id attribute, returns a random entity reference."
  identity)

(defmethod random-ref* nil
  [_]
  (throw (js/Error "Ref must have unique id attribute")))

(defmethod random-ref* :default
  [id-attr]
  [id-attr (random-uuid)])

(defn random-ref
  [id-attr & {:keys [label provisional]}]
  (cond-> (random-ref* id-attr)
    label       (update 1 vary-meta assoc :label label)
    provisional (update 1 vary-meta assoc :provisional true)))

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

(defn transient?
  "Returns true if the given entity references is transient."
  [ref]
  (boolean
   (some-> ref norm/ref-meta :transient)))

(defn provisional-ref?
  "Returns true if the given entity references is provisional."
  [ref]
  (boolean
   (some-> ref norm/ref-meta :provisional)))

(defn get-label
  "Returns any metadata label associated with the given entity
  reference."
  [ref]
  (some-> ref norm/ref-meta :label))

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
    :component/uuid})

(defn new-db
  "Returns a new db, optionally with initial data and unique id
  attributes. Must have at least one unique id attribute."
  ([]
   (new-db {}))
  ([data]
   (new-db data default-unique-attributes))
  ([data id-attrs]
   {:pre [(map? data) (seq id-attrs)]}
   (assoc data ::id-attrs id-attrs)))

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

;;;; Write API

(defn- touch-queries*
  [u-fn index e-ref]
  (if-let [queries (e->q index e-ref)]
    (u-fn index ::touched-queries set/union queries)
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
    (log/warn "Writing to unmounted transient state" ref)))

(defn mergen
  "Normalizes the given tx data, merges all normalized entities into the
  db, touches any queries tracking those entities in the index, and
  increments the db and index ticks."
  [{:keys [::id-attrs] :as db} tx]
  (let [opts {:id-attrs id-attrs}]
    (loop [data       (transient (::data db {}))
           index      (transient (::index db {}))
           [e & more] (->> (norm/to-many tx id-attrs)
                           (filter map?)
                           (mapcat #(norm/normalize % opts)))]
      (if e
        (let [ref (norm/refer-one e opts)]
          (warn-on-transient-write ref)
          (recur (t/update! data ref merge e)
                 (touch-queries! index ref)
                 more))
        (-> db
            (assoc ::data (persistent! data))
            (assoc ::index (persistent! index))
            inc-tick)))))

(defn assoc-inn
  "Adds the unnormalized value to entity at path. Similar semantics to
  assoc-in, where the entity ref is first part of path. Additionally
  touches any queries that are tracking the entity and increments the
  db and index ticks. Does not resolve entity references, so it cannot
  do deep updates in normalized data."
  [{:keys [::id-attrs] :as db} [ref :as path] value]
  {:pre [(norm/ref? ref id-attrs)]}
  (warn-on-transient-write ref)
  (-> db
      (assoc-in (cons ::data path) value)
      (update ::index touch-queries ref)
      inc-tick))

(defn dissocn
  "Removes the list of entities from the db, touches any queries
  tracking those entities in the index, and increments the db and
  index ticks. Does not remove any nested entities in tx."
  [{:keys [::id-attrs] :as db} & tx]
  (let [opts {:id-attrs id-attrs}]
    (loop [data         (transient (::data db {}))
           index        (transient (::index db {}))
           [ref & more] (or (norm/refer-many tx opts)
                            (when (every? keyword? tx)
                              tx))]
      (if ref
        (recur (dissoc! data ref)
               (touch-queries! index ref)
               more)
        (-> db
            (assoc ::data (persistent! data))
            (assoc ::index (persistent! index))
            inc-tick)))))

(defn update-inn
  "Updates the value at the normalized db path. Similar semantics to
  clojure.core/update-in, where the entity ref or link attribute is
  first part of path. The updated value is either an entity, a link
  attribute value, or an attribute of an entity. Does not resolve
  entity references, so it cannot do deep updates in normalized data."
  [{:keys [::id-attrs] :as db} [ref :as path] f & args]
  {:pre [(or (and (keyword? ref)
                  (= (count path) 1))
             (and (norm/ref? ref id-attrs)
                  (<= (count path) 2)))]}
  (warn-on-transient-write ref)
  (-> (apply update-in db (cons ::data path) f args)
      (update ::index touch-queries ref)
      inc-tick))

(defn assocn
  "Adds a normalized link at the given attribute."
  [{:keys [::id-attrs] :as db} attr tx]
  {:pre [(identity attr)]}
  (letfn [(assocn* [refs]
            (-> db
                (mergen tx)
                (assoc-in [::data attr] refs)
                (update ::index touch-queries attr)))]
    (let [opts {:id-attrs id-attrs}]
      (if-let [ref (norm/refer-one tx opts)]
        (assocn* ref)
        (if-let [refs (norm/refer-many tx opts)]
          (assocn* (norm/like tx refs))
          db)))))

;;;; Query API

(declare pull*)

(defn- link?
  [{:keys [ref]} expr]
  (and (map? expr) (not ref)))

(defn- wildcard?
  [expr]
  (= expr '*))

(defn- pull-pattern
  [{:keys [db ref acc-refs!] :as context} pattern result]
  (let [c (assoc context
            :entity (get db ref)
            :pattern pattern)]
    (when (and acc-refs! ref)
      (acc-refs! ref))
    (reduce (fn [result expr]
              (pull* c expr result))
            result
            (distinct pattern))))

(defn- pull-sync!
  "Parses sync expression and dispatches sync init."
  [{:keys [db ref sync-start!] :as context} sync result]
  (let [[id expr & args] (case (first sync) ; Account for quoted list expressions
                           list (rest sync)
                           sync)]
    ;; Only dispatch side-effects if they have been explicitly
    ;; configured. The default pull implementation should be pure.
    (when sync-start!
      (sync-start! {:id   id
                    :db   db
                    :ref  ref
                    :args args}))
    (pull* context expr result)))

(defn- pull-attr
  [{:keys [entity]} attr result]
  (let [v (get entity attr)]
    (if (some? v)
      (assoc result attr v)
      result)))

(defn- pull-join
  [{:keys [entity pattern id-attrs provisional] :as context} join result]
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
                     (or (not provisional)
                         (provisional-ref? value)))
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
  [{:keys [db acc-refs!] :as context} link result]
  (let [[[attr]] (seq link)]
    (when (and acc-refs! attr)
      (acc-refs! attr))
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
  ([context expr result]
   (cond
     (link? context expr) (pull-link context expr result)
     (list? expr)         (pull-sync! context expr result)
     (vector? expr)       (pull-pattern context expr result)
     (keyword? expr)      (pull-attr context expr result)
     (wildcard? expr)     (pull-wildcard context expr result)
     (map? expr)          (pull-join context expr result)
     :else                (throw (ex-info "Invalid pull expression"
                                          {::expr expr})))))

(defn pull!
  "For any `:db` provided in `context`, evaluates the pull expression
  specified by `expr`. By default, this implementation is a pure
  function of the given `:db` value. However, if `:acc-refs!` and
  `:sync-start!` fns are provided in `context`, this implementation
  accumulates touched entity references, and dispatches sync
  expressions via side-effects. This means the entire impl must also
  be eager. This function is not part of the api and not meant to be
  used directly."
  [{ref :ref :as context} expr]
  (letfn [(get-attr [expr]
            (when ref
              (cond
                (map? expr)     (ffirst expr)
                (keyword? expr) expr
                (list? expr)    (get-attr (second expr))
                :else           nil)))]
    (if-let [attr (get-attr expr)]
      (get (pull* context [expr]) attr)
      (pull* context expr))))

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

(defn pull-provisional
  [{:keys [::data ::id-attrs]} e-ref]
  (pull! {:id-attrs    id-attrs
          :db          data
          :ref         e-ref
          :provisional true}
         [{'* '...}]))

(defn pull
  "Pulls normalized data from the given db. This is a non-reactive,
  functionally pure version of pull! for use in event handlers. The
  data is pulled from the entity according to a pull pattern that
  conforms to:
     { :attr sub-pattern }
           If the entity contains :attr, it is treated as an
           entity reference and resolved. The algorithm then pulls
           the sub-pattern from the resolved entity.
     [ sub-pattern-1 sub-pattern-2 sub-pattern-3 ]
           The algorithm attempts to pull each subpattern from the
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
  ([{:keys [::data ::id-attrs] :as db} expr e-ref]
   (pull! {:id-attrs id-attrs
           :db       data
           :ref      e-ref}
          expr)))

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

(defn- acc-refs!
  "Accumulates freshly touched entity refs in volatiles."
  [fresh stale e-ref]
  (vswap! fresh conj e-ref)
  (vswap! stale disj e-ref))

(defn- pull-reactive
  "Given a pull expression and an entity reference, returns the
  de-normalized entity data from the database. Additionally adds newly
  tracked entities to the query index, updates the given query tick
  for the given query, clears stale entities from the previous pull,
  and clears touched queries."
  [{:keys [db index expr e-ref q-ref query-tick sync-start!]}]
  (let [fresh  (volatile! #{})
        stale  (volatile! (q->e index q-ref #{}))
        result (pull! {:id-attrs    (::id-attrs db)
                       :db          (::data db)
                       :ref         e-ref
                       :acc-refs!   (partial acc-refs! fresh stale)
                       :sync-start! sync-start!}
                      expr)]
    {:db     db
     :result result
     :index  (-> index
                 (assoc-in [::q->e q-ref] @fresh)
                 (update ::e->q clear-stale-entities @stale q-ref)
                 (update ::e->q add-fresh-entities @fresh q-ref)
                 (assoc-in [::q->tick q-ref] query-tick)
                 (assoc ::touched-queries #{}))}))

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

(def inject-index
  "Injects query index into app state for normalized data fns to work."
  (f/->interceptor
   :id ::inject-index
   :before inject-index-before
   :after inject-index-after))

;;;; Fx

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

(f/reg-sub-raw ::query-tick
  ;; Returns the given query's tick, syncing to the db tick when the
  ;; query must re-run. The query must re-run whenever it has been
  ;; touched, it is uninitialized, or the db tick and index ticks are
  ;; out of sync, indicating time-travel.
  (fn [_ [_ q-ref]]
    (let [db-tick (f/subscribe [::db-tick])]
      (r/reaction
        (let [index      (.-state query-index)
              touched    (::touched-queries index)
              index-tick (::tick index)
              query-tick (q->tick index q-ref)]
          (if (or (contains? touched q-ref)
                  (nil? query-tick)
                  (not= @db-tick index-tick))
            @db-tick                    ; Next query tick = db tick
            query-tick))))))

(defn reactive?
  [x]
  (satisfies? r/IReactiveAtom x))

(defn maybe-deref
  [^js x]
  (cond-> x
    (reactive? x) deref))

(defn result-query-v
  [[id & args]]
  (let [ns (namespace id)
        n  (name id)]
    (-> (keyword ns (str n "[result-fn]"))
        (cons args)
        vec)))

(defn result-reaction
  [input-r query-v result-fn]
  (let [query-v* (result-query-v query-v)
        r-id     (atom nil)
        r        (r/reaction
                   (trace/with-trace
                     {:operation (first query-v*)
                      :op-type   :sub/run
                      :tags      {:query-v  query-v*
                                  :reaction @r-id}}
                     (let [res (->> query-v*
                                    (map maybe-deref)
                                    (apply result-fn @input-r))]
                       (trace/merge-trace! {:tags {:value res}})
                       res)))]
    (->> r
         interop/reagent-id
         (reset! r-id))
    r))

(defn pull-reaction
  "Returns a differential pull reaction. `expr-rn` is a function that
  given query vector arguments, returns a pull query spec and
  optionally an entity ref that is being queried. `query-v` is the
  query vector supplied to the generating subscription. The tracing of
  this reaction was patterned after the `re-frame.sub/reg-sub`
  implementation, and allows subscriptions based on these reactions to
  be inspected in `re-frame-10x`. Note that while the computation in
  the pull reaction is dependent on the db and query index values, it
  is not reactive to them. Instead, it is reactive to changes the
  query tick, which tracks the db-tick and synced."
  [query-v expr-fn & [config]]
  (let [q-ref  (random-ref :query/uuid)
        q-tick (f/subscribe [::query-tick q-ref])
        r-id   (atom nil)
        r      (r/make-reaction
                 (fn []
                   (let [[id & args]  query-v
                         [expr e-ref] (apply expr-fn args)]
                     (trace/with-trace
                       {:operation id
                        :op-type   :sub/run
                        :tags      {:query-v  query-v
                                    :reaction @r-id}}
                       (let [in (merge
                                 config
                                 {:db         (.-state db/app-db)
                                  :index      (.-state query-index)
                                  :expr       expr
                                  :e-ref      e-ref
                                  :q-ref      q-ref
                                  :query-tick @q-tick})
                             
                             {i   :index
                              res :result} (pull-reactive in)]
                         (reset! query-index i)
                         (trace/merge-trace! {:tags {:value res}})
                         res))))
                 :on-dispose
                 (fn []
                   (swap! query-index dispose-query q-ref)
                   (when-let [f (:on-dispose config)]
                     (f))))]
    (->> r
         interop/reagent-id
         (reset! r-id))
    r))

;; Transactions and entities

(def tx-walker
  (sp/recursive-path [] p
    (sp/continue-then-stay
     (sp/cond-path
       map?        sp/MAP-VALS
       sequential? sp/ALL
       set?        sp/ALL)
     p)))

(def entity-walker
  [tx-walker
   (fn [x]
     (and (map? x) (:kr/type x)))])

(defn e
  [m ref]
  (apply util/assoc-nil m ref))

(defn new
  [m]
  (letfn [(f [x]
            (cond-> x
              (not (:system/uuid x)) (e (random-ref :system/uuid :provisional true))))]
    (sp/transform entity-walker f m)))

;;;; Utils

(defn db-label-filter
  "Returns a version of the re-frame db filtered by label."
  [& [label]]
  (letfn [(f [[ref _]]
            (when (vector? ref)
              (cond-> (get-label ref)
                label (= label))))]
    (->> @db/app-db
         ::data
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

