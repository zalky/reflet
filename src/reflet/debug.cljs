(ns reflet.debug
  (:require [re-frame.core :as f]
            [reagent.core :as r]
            [reflet.db :as db]))

(f/reg-event-db ::tap
  ;; ::d/tap must happen after the ::d/tap-cleanup of the previous
  ;; react lifecycle. To guarantee this, ::d/tap must be invoked in
  ;; either the `:ref` callback, or the `:component-did-mount` phase
  ;; of the component lifecycle. Must not dispatch in a `with-let`,
  ;; where it will happen during the first render.
  [db/inject-query-index]
  (fn [db [_ ref tap]]
    (-> db
        (update-in [::taps ref] merge tap)
        (db/mergen tap))))

(f/reg-event-db ::tap-cleanup
  [db/inject-query-index]
  (fn [db [_ ref]]
    (-> db
        (update ::taps dissoc ref)
        (db/dissocn ref))))

(f/reg-sub ::taps
  (fn [db _]
    (get db ::taps)))

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

