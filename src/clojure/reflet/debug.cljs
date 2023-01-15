(ns reflet.debug
  (:require [cinch.core :as util]
            [re-frame.core :as f]
            [re-frame.db :as rdb]
            [re-frame.events :as events]
            [re-frame.interceptor :as i]
            [re-frame.interop :as interop]
            [re-frame.registrar :as reg]
            [reagent.core :as r]
            [reflet.db :as db]))

(defn- tap-cofx-before
  [context]
  (assoc-in context [:coeffects :db] @rdb/app-db))

(defn- tap-fx-after
  [{{index ::db/index
     db    :db} :effects}]
  (when (and db (not (identical? @rdb/app-db db)))
    (reset! rdb/app-db db))
  (when index
    (reset! db/query-index index)))

(def ^:private tap-fx
  (f/->interceptor
   :id ::tap-interceptor
   :before tap-cofx-before
   :after tap-fx-after))

(defn- tap-handler-before
  [handler]
  (fn [{{e  :event
         db :db} :coeffects
        :as      context}]
    (->> (handler db e)
         (assoc-in context [:effects :db]))))

(defn- tap-handler
  [handler]
  (f/->interceptor
   :id     ::do-handler
   :before (tap-handler-before handler)))

(defn- reg-tap-event
  [id handler]
  (->> [tap-fx
        db/inject-query-index
        (tap-handler handler)]
       (events/register id)))

(defn disp-tap
  "Manually schedules and dispatches debugger tap events in order to
  avoid polluting the re-frame trace. This is a consequence of a
  couple of things:

  1. Re-frame does not allow contextual disabling of tracing.

  2. Reactive positioning of the purple tap marks is implemented using
     a periodic poll of the tap target. Unfortunately, a non-polling
     based method to observing changes in the position of the element
     being tapped is quite complex. It involves placing multiple event
     listeners all the way up the DOM tree from the tap point, as well
     as managing resize observers, with numerous edge cases
     everywhere. Popper.js implementation works through all these, but
     we do not want to use that as it introduces a complex JS
     dependency, which would need to be inlined and maintained over
     time. Nobody loves a polling solution, but given how infrequently
     we poll (1 Hz), and that there are no apparent edge cases, it is
     by far the most robust solution. Also remember that this solution
     is only used for the position of the purple tap marks. Literally
     everything else about taps is reactive using normal, efficient
     approaches.

  Note that this dispatch fn does not work for any other event
  but ::tap."
  [event]
  {:pre [(= (first event) ::tap)]}
  (interop/next-tick
   (fn []
     (let [h (reg/get-handler events/kind ::tap)]
       (-> (i/context event h)
           (i/invoke-interceptors :before)
           (i/change-direction)
           (i/invoke-interceptors :after))))))

(reg-tap-event ::tap
  ;; ::d/tap must happen after the ::d/untap of the previous react
  ;; lifecycle. To guarantee this, ::d/tap must be invoked in either
  ;; the `:ref` callback, or the `:component-did-mount` phase of the
  ;; component lifecycle. Must not dispatch in a `with-let`, where it
  ;; will happen during the first render. Untap is implemented via the
  ;; reflet.core/cleanup :debug/id defmethod.
  (fn [db [_ tap]]
    (let [ref (find tap :debug/id)]
      (when (-> (db/getn db ref)
                (select-keys (keys tap))
                (not= tap))
        (-> db
            (db/updaten ::taps util/conjs ref)
            (db/mergen tap))))))

(f/reg-sub ::e->events
  (constantly (r/cursor db/trace-index [::db/e->event]))
  (fn [trace [_ ref]]
    (-> trace
        (get ref)
        (reverse)
        (not-empty))))

(f/reg-sub ::fsm->transitions
  (constantly (r/cursor db/trace-index [::db/fsm->transition]))
  (fn [trace [_ ref]]
    (->> (get trace ref)
         (vals)
         (map (partial sort-by :t #(compare %2 %1)))
         (not-empty))))

(f/reg-sub ::e->queries
  (fn [_]
    [(r/cursor db/query-index [::db/e->q])
     (r/cursor db/query-index [::db/q->trace])])
  (fn [[e->q q->trace] [_ ref]]
    (->> (get e->q ref)
         (map (partial get q->trace))
         (mapcat vals)
         (map (partial sort-by :t #(compare %2 %1)))
         (not-empty))))

