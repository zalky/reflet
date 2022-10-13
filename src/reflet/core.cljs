(ns reflet.core
  "Re-defines re-frame api and provides convenience utilites."
  (:refer-clojure :exclude [uuid])
  (:require [cljs.spec.alpha :as s]
            [re-frame.core :as f]
            [re-frame.interop :as interop]
            [re-frame.registrar :as reg]
            [reagent.core :as r]
            [reagent.impl.component :as util**]
            [reflet.db :as db]
            [reflet.fsm :as fsm]
            [reflet.interceptors :as itor]
            [reflet.interop :as i]
            [reflet.ref-spec :as rs]

            ;; Required for macro use.
            [cinch.core])
  (:require-macros [reflet.core]))

;;;; Re-frame API

(def reflet-interceptors
  [db/inject-index
   itor/add-global-interceptors
   fsm/fsm-lifecycle-interceptor])

(defn reg-event-db
  ([id handler]
   (reg-event-db id nil handler))
  ([id interceptors handler]
   (f/reg-event-db id
     [reflet-interceptors interceptors]
     handler)))

(defn reg-event-fx
  ([id handler]
   (reg-event-fx id nil handler))
  ([id interceptors handler]
   (f/reg-event-fx id
     [reflet-interceptors interceptors]
     handler)))

(defn reg-event-ctx
  ([id handler]
   (reg-event-ctx id nil handler))
  ([id interceptors handler]
   (f/reg-event-ctx id
     [reflet-interceptors interceptors]
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

;;;; Reflet API

(def ^:dynamic *force-persistent-refs*
  "Only use during reflet.fixtures/run-test-async tests. All refs during
  async tests are made to be persistent so that when the async test
  falls out of the `with-ref` scope, the app state is not cleaned up
  prematurely. You should not use this with a running application."
  false)

(defn reg-config
  "Registers reflet configuration map. The configuration should be
  registered before application boot. Currently supported
  configuration options:

  :sync-start!
            Sync expressions within pull syntax are dispatched via a
            sync-start!. Default is nil, which means sync expressions
            are ignored."
  [config]
  (reg/register-handler ::config ::config config))

(reg-fx ::reg-config reg-config)

(defn reg-expr-fn
  [id expr-fn]
  (reg/register-handler ::expr-fn id expr-fn))

(defn reg-result-fn
  [id result-fn]
  (reg/register-handler ::result-fn id result-fn))

(defn get-config
  []
  (reg/get-handler ::config ::config))

(defn get-expr-fn
  [id]
  (or (reg/get-handler ::expr-fn id)
      (throw (ex-info "expr-fn not defined" {:id id}))))

(defn get-result-fn
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
   (-> (get-config)
       (db/pull-reaction expr-fn query-v)
       (result-reaction query-v))))

(defn reg-pull*
  "Prefer reg-pull macro."
  ([id expr-fn]
   (reg-pull* id expr-fn nil))
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
  "Composes a series of named reactions, where the result of each
  reaction in the sequence is provided as input to the next. Semantics
  are similar to `clojure.core/comp`. Except for the first, every
  other reaction in the pipeline should expect only a single argument.
  As with comp, the order of operations is reversed from the order in
  which they are declared. The resultant reaction returned by the
  composition is cached according to the input arguments of the
  pipeline. No intermediary reactions are cached."
  [id comp-ids]
  (let [[r1-id & ids] (reverse comp-ids)]
    (reg-sub-raw id
      (fn [_ query-v]
        (let [r1 (->> (rest query-v)
                      (cons r1-id)
                      (vec)
                      (pull-reaction))]
          (reduce reg-comp-rf r1 ids))))))

(reg-cofx :random-ref
  ;; This cofx has the same semantics as with-ref, with the exception
  ;; that refs cannot be transient, because there is no reactive
  ;; context in event handlers.
  (fn [cofx {:keys [meta]
             :as   bindings}]
    (letfn [(rf [m {:keys [key id-attr]}]
              (->> (db/random-ref id-attr meta key)
                   (assoc m key)))]
      (->> bindings
           (s/conform ::rs/bindings)
           (reduce rf {})
           (assoc cofx :random-ref)))))

(reg-event-fx ::with-ref-cleanup
  (fn [{:keys [db]} [_ & refs]]
    {::i/cleanup refs
     :db         (apply db/dissocn db refs)
     :log        (concat [:debug "Entity cleanup"] refs)}))

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

(reg-fx :stop-prop
  (fn [e]
    (.stopPropagation e)))

(reg-fx :prevent-default
  (fn [e]
    (.preventDefault e)))

;; Debugger

(defonce debugger
  (r/atom nil))

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
    (let [new (r/props this)
          old (util**/extract-props old-argv)]
      (f old new))))
