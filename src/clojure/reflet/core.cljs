(ns reflet.core
  "Reflet API and convenience utilities."
  (:refer-clojure :exclude [uuid])
  (:require [cljs.spec.alpha :as s]
            [re-frame.core :as f]
            [re-frame.interop :as interop]
            [re-frame.loggers :as logf]
            [re-frame.registrar :as reg]
            [reagent.core :as r]
            [reagent.impl.component :as util]
            [reflet.config :as config]
            [reflet.db :as db]
            [reflet.debug :as d]
            [reflet.fsm :as fsm]
            [reflet.interceptors :as itor]
            [reflet.interop :as i]
            [reflet.log :as log]
            [reflet.ref-spec :as rs]

            ;; Required for macro use.
            [cinch.core])
  (:require-macros [reflet.core]))

;;;; Re-frame API

(def reflet-interceptors
  [db/inject-query-index
   db/trace-event
   fsm/advance
   (itor/add-global-interceptors ::fsm/fsm)
   (itor/add-global-interceptors)])

(defn reg-event-db
  ([id handler]
   (reg-event-db id nil handler))
  ([id interceptors handler]
   (itor/reg-event-db-impl id
     reflet-interceptors interceptors
     handler)))

(defn reg-event-fx
  ([id handler]
   (reg-event-fx id nil handler))
  ([id interceptors handler]
   (itor/reg-event-fx-impl id
     reflet-interceptors interceptors
     handler)))

(defn reg-event-ctx
  ([id handler]
   (reg-event-ctx id nil handler))
  ([id interceptors handler]
   (itor/reg-event-ctx-impl id
     reflet-interceptors interceptors
     handler)))

(def reg-sub     f/reg-sub)
(def reg-sub-raw f/reg-sub-raw)
(def sub         f/subscribe)
(def disp        f/dispatch)
(def disp-sync   f/dispatch-sync)

;;;; Reflet API

(defn debug?
  "Returns true if debugging is activated."
  []
  (boolean db/tap-fn))

(def pull-fx db/pull-fx)

(defmethod pull-fx :default
  [{id :id} _]
  (logf/console :warn "no pull-fx defined for:" id))

(f/reg-event-fx ::config
  (fn [{db :db} [_ {id-attrs :id-attrs
                    dispatch :dispatch
                    :as      config}]]
    (cond-> {:db             (db/new-db db id-attrs)
             ::config/config config}
      dispatch (assoc :dispatch dispatch))))

(defn- reg-expr-fn
  [id expr-fn]
  (reg/register-handler ::expr-fn id expr-fn))

(defn- reg-result-fn
  [id result-fn]
  (reg/register-handler ::result-fn id result-fn))

(defn- get-expr-fn
  [id]
  (or (reg/get-handler ::expr-fn id)
      (throw (ex-info "expr-fn not defined" {:id id}))))

(defn- get-result-fn
  [id]
  (reg/get-handler ::result-fn id))

(defn result-reaction
  [input-r [id :as query-v]]
  (if-let [f (get-result-fn id)]
    (db/result-reaction input-r f query-v)
    input-r))

(defn pull-reaction
  ([[id :as query-v]]
   (-> (get-expr-fn id)
       (pull-reaction query-v)))
  ([expr-fn query-v]
   (-> (config/get-config)
       (db/pull-reaction expr-fn query-v)
       (result-reaction query-v))))

(defn reg-pull-impl
  "Do not use, see reg-pull macro."
  ([id expr-fn]
   (reg-pull-impl id expr-fn nil))
  ([id expr-fn result-fn]
   (reg-expr-fn id expr-fn)
   (when result-fn (reg-result-fn id result-fn))
   (reg-sub-raw id
     (fn [_ query-v]
       (pull-reaction query-v)))))

(defn- reg-comp-rf
  [r id]
  (let [query-v [id r]]
    (-> (get-expr-fn id)
        (comp deref)
        (pull-reaction query-v))))

(defn reg-comp
  "Composes a series of named, normalized reactions, where the result of
  each reaction in the sequence is provided as input to the
  next. Semantics are similar to `clojure.core/comp`. Except for the
  first, every other reaction in the pipeline should expect only a
  single argument.  As with comp, the order of operations is reversed
  from the order in which they are declared. The resultant reaction
  returned by the composition is cached according to the input
  arguments of the pipeline. No intermediary reactions are cached."
  [id comp-ids]
  (let [[r1-id & ids] (reverse comp-ids)]
    (reg-sub-raw id
      (fn [_ query-v]
        (let [r1 (->> (rest query-v)
                      (cons r1-id)
                      (vec)
                      (pull-reaction))]
          (reduce reg-comp-rf r1 ids))))))

(f/reg-cofx ::with-ref
  ;; This cofx has the same semantics as with-ref, except that here
  ;; refs cannot be transient, because there is no reactive
  ;; context in event handlers.
  (fn [cofx {:keys [meta]
             :as   bindings}]
    (letfn [(rf [m {:keys [key id-attr]}]
              (->> (db/random-ref id-attr meta key)
                   (assoc m key)))]
      (->> bindings
           (s/conform ::rs/bindings)
           (reduce rf {})
           (assoc cofx ::with-ref)))))

(defmulti cleanup
  "Dispatches on the ref's unique attribute."
  (fn [cofx [_ ref]]
    (first ref)))

(defn- untap
  [db ref]
  (db/updaten db ::d/taps disj ref))

(defmethod cleanup :debug/id
  ;; Cleanup behaviour is specific to the debugger.
  [{db :db :as cofx} [_ ref :as event]]
  (let [handler    (get-method cleanup :default)
        default-fx (handler cofx event)]
    (->> {::log/log [:debug "Debug cleanup" ref]
          :db       (untap (:db default-fx db) ref)}
         (merge default-fx))))

(defmethod cleanup :el/uuid
  [cofx [_ ref :as event]]
  {::log/log   [:debug "DOM cleanup" ref]
   ::i/cleanup ref})

(defmethod cleanup :js/uuid
  [cofx [_ ref :as event]]
  {::log/log   [:debug "JS cleanup" ref]
   ::i/cleanup ref})

(defmethod cleanup :default
  [{db :db} [_ ref]]
  {::log/log        [:debug "Reactive state cleanup" ref]
   :db              (db/dissocn db ref)
   ::db/unmount-ref ref})

(def cleanup-interceptors
  [db/inject-query-index
   fsm/advance
   (itor/add-global-interceptors ::fsm/fsm)
   (itor/add-global-interceptors)])

(itor/reg-event-fx-impl ::cleanup
  cleanup-interceptors
  cleanup)

;;;; Additional Utilities

(defn reg-no-op
  "Convenience function for declaring no-op events."
  [& ids]
  (doseq [id ids]
    (reg-event-fx id
      (constantly nil))))

(defn stop-prop
  [f]
  (fn [e]
    (.stopPropagation e)
    (f e)))

(defn prevent-default
  [f]
  (fn [e]
    (.preventDefault e)
    (f e)))

(f/reg-fx ::stop-prop
  (fn [e]
    (.stopPropagation e)))

(f/reg-fx ::prevent-default
  (fn [e]
    (.preventDefault e)))

(defonce debounced-events
  (atom {}))

(defn disp-debounced
  "Debounces dispatched events by id."
  [{[id :as event] :dispatch
    ms             :ms}]
  {:pre [event ms]}
  (let [debounce-id (random-uuid)]
    (letfn [(disp-fn []
              (when (= debounce-id (get @debounced-events id))
                (swap! debounced-events dissoc id)
                (disp event)))]
      (swap! debounced-events assoc id debounce-id)
      (interop/set-timeout! disp-fn ms))))

(f/reg-fx ::disp-debounced
  disp-debounced)

(defn props-handler
  "Given an update handler, f, that accepts a single component argument,
  returns lifecycle handler function that will invoke the provided fn
  with component props. Careful, other lifecycles have different
  signatures with respect to new and old values during an update, and
  are not interchangeable."
  [f]
  (fn [this]
    (f (r/props this))))

(defn props-did-update-handler
  "Given an update handler, f, returns a componentDidUpdate lifecycle
  handler function that will invoke the provided fn with old and new
  component props when they change. The given handler, f ,must be a
  function of two arguments, the old props value, and the new props
  value. Careful, other lifecycles have different signatures with
  respect to new and old values during an update, and are not
  interchangeable."
  [f]
  (fn [this old-argv]
    (let [new (r/props this)
          old (util/extract-props old-argv)]
      (f old new))))
