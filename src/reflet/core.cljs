(ns reflet.core
  "Re-defines Re-frame api and provides convenience utilites."
  (:refer-clojure :exclude [uuid])
  (:require [cljs.spec.alpha :as s]
            [re-frame.core :as f]
            [re-frame.interop :as interop]
            [re-frame.registrar :as reg]
            [reagent.core :as reagent]
            [reagent.ratom :as r]
            [reagent.impl.component :as util**]
            [reflet.db :as db]
            [reflet.interceptors :as itor]
            [reflet.interop :as i]
            [reflet.ref-spec :as ref-spec])
  (:require-macros [reflet.core]))

(defn ^:dynamic *random-ref*
  "This is only meant to be referenced or rebound from this namespace
  and the corresponding test namespace. In all other namespaces prefer
  the :random-ref co-effect via inject-cofx."
  ([k] (db/random-ref k))
  ([k _] (db/random-ref k)))

(f/reg-cofx :random-ref
  (fn [cofx bindings]
    (->> bindings
         (s/conform ::ref-spec/binding-map)
         (reduce (fn [m {:keys [key attr]}]
                   (assoc m key (*random-ref* attr)))
                 {})
         (assoc cofx :random-ref))))

;;;; Re-frame API

(defn reg-event-db
  "Overloaded re-frame event registration injects global
  interceptors."
  ([id handler]
   (reg-event-db id nil handler))
  ([id interceptors handler]
   (f/reg-event-db id
     [db/inject-index itor/add-global-interceptors interceptors]
     handler)))

(defn reg-event-fx
  "Overloaded re-frame event registration injects global
  interceptors."
  ([id handler]
   (reg-event-fx id nil handler))
  ([id interceptors handler]
   (f/reg-event-fx id
     [db/inject-index itor/add-global-interceptors interceptors]
     handler)))

(defn reg-event-ctx
  "Overloaded re-frame event registration injects global
  interceptors."
  ([id handler]
   (reg-event-ctx id nil handler))
  ([id interceptors handler]
   (f/reg-event-ctx id
     [db/inject-index itor/add-global-interceptors interceptors]
     handler)))

(def dispatch                   f/dispatch)
(def dispatch-sync              f/dispatch-sync)
(def subscribe                  f/subscribe)

(def reg-sub                    f/reg-sub)
(def reg-sub-raw                f/reg-sub-raw)
(def clear-sub                  f/clear-sub)
(def clear-subscription-cache!  f/clear-subscription-cache!)

(def reg-fx                     f/reg-fx)
(def clear-fx                   f/clear-fx)

(def reg-cofx                   f/reg-cofx)
(def inject-cofx                f/inject-cofx)
(def clear-cofx                 f/clear-cofx)

(def debug                      f/debug)
(def path                       f/path)
(def enrich                     f/enrich)
(def trim-v                     f/trim-v)
(def after                      f/after)
(def on-changes                 f/on-changes)

(def ->interceptor              f/->interceptor)
(def get-coeffect               f/get-coeffect)
(def assoc-coeffect             f/assoc-coeffect)
(def get-effect                 f/get-effect)
(def assoc-effect               f/assoc-effect)
(def enqueue                    f/enqueue)

(def set-loggers!               f/set-loggers!)
(def console                    f/console)
(def make-restore-fn            f/make-restore-fn)
(def purge-event-queue          f/purge-event-queue)
(def add-post-event-callback    f/add-post-event-callback)
(def remove-post-event-callback f/remove-post-event-callback)

;;;; Aria entity model API

(defn- assert-get-handler
  [kind id]
  (or (reg/get-handler kind id)
      (throw
       (ex-info
        "Could not find handler"
        {:kind kind
         :id   id}))))

(defn wrap-result-reaction
  "Given an input reaction, and a query vector, returns a reaction
  wrapped with a result computation, if it exists."
  [input-r [id :as query-v]]
  (if-let [f (reg/get-handler :result-fn id)]
    (db/result-reaction input-r query-v f)
    input-r))

(defn reg-config
  "Sync expressions within pull syntax are dispatched via a global
  sync-start! fn that can be configured on application boot. If not
  configured it is simply ignored."
  [config]
  (reg/register-handler ::config ::config config))

(reg-fx ::reg-config reg-config)

(defn synced-pull-reaction
  [query-v expr-fn]
  (->> (reg/get-handler ::config ::config)
       (db/pull-reaction query-v expr-fn)))

(defn pull-reaction-handler
  [_ [id :as query-v]]
  (let [expr-fn (assert-get-handler :expr-fn id)]
    (-> query-v
        (synced-pull-reaction expr-fn)
        (wrap-result-reaction query-v))))

(defn reg-pull*
  "Registers a named pull query. Semantics are datomic pull, with the
  addition of link queries and attribute accessing. Link queries allow
  the user to query the db for a global named attribute reference in
  the db, rather than a UUID based entity reference. Attribute
  accessing is just a convenience for returning the value of a
  specific attribute, rather than an entity map containing the
  attribute entry."
  ([id expr-fn]
   (reg-pull* id expr-fn nil))
  ([id expr-fn result-fn]
   (reg/register-handler :expr-fn id expr-fn)
   (when result-fn
     (reg/register-handler :result-fn id result-fn))
   (reg-sub-raw id pull-reaction-handler)))

(defn reg-comp
  "Composes a series of named reactions, where the result of each
  reaction in the sequence is provided as input to the next. Semantics
  are similar to `clojure.core/comp`, in that aside from the first
  reaction, each reaction in the pipeline should expect only a single
  argument. Like `comp`, the order of operations is also reverse the
  order in which they are listed. Note that reactions that participate
  in compositions are not uniquely identified by their query vector
  arguments, but by their input reactions. This is in contrast to
  reactions produced by `reg-pull`, `reg-sub` or `reg-sub-raw`, which
  are uniquely identified by query vector used to construct them. This
  has implications for subscription caching: while the overall
  reaction pipeline that is returned by `reg-comp` is cached by
  `re-frame.core/subscribe`, the constituent reactions are by their
  nature always distinct between two different compositions."
  [id comp-ids]
  (let [[r1-id & ids] (reverse comp-ids)]
    (letfn [(comp-r [r id]
              (let [expr-fn  (assert-get-handler :expr-fn id)
                    expr-fn* (comp expr-fn deref)
                    query-v  [id r]]
                (-> query-v
                    (synced-pull-reaction expr-fn*)
                    (wrap-result-reaction query-v))))]
      (reg-sub-raw id
        (fn [_ [_ & args]]
          (let [query-v (vec (cons r1-id args))
                r1      (pull-reaction-handler nil query-v)]
            (reduce comp-r r1 ids)))))))

(reg-event-fx ::with-ref-cleanup
  (fn [{:keys [db]} [_ & refs]]
    {::i/cleanup refs
     :db         (apply db/dissocn db refs)
     :log        (concat [:debug "Entity cleanup"] refs)}))

(deftype IResubscribe [sub-fn]
  IDeref
  (-deref [this]
    (deref (sub-fn))))

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

(f/reg-fx :stop-prop
  (fn [e]
    (.stopPropagation e)))

(f/reg-fx :prevent-default
  (fn [e]
    (.preventDefault e)))

;;;; Debounced dispatch

(defonce debounced-events
  (atom {}))

(f/reg-fx :dispatch-debounced
  (fn [{[event-id :as event] :dispatch
        ms                   :ms}]
    (let [debounce-id (random-uuid)]
      (letfn [(dispatch-debounced []
                (when (= debounce-id (get @debounced-events event-id))
                  (dispatch event)))]
        (swap! debounced-events assoc event-id debounce-id)
        (interop/set-timeout! dispatch-debounced ms)))))

(defn props-did-update-handler
  "Returns a componentDidUpdate lifecycle handler function that will
  invoke the provided fn with old and new component props when they
  change. Careful, different lifecycles have different signatures with
  respect to new and old values during an update, and are not
  interchangeable."
  [f]
  (fn [this old-argv]
    (let [new (reagent/props this)
          old (util**/extract-props old-argv)]
      (f old new))))
