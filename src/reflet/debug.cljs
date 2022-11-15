(ns reflet.debug
  (:require [cinch.core :as util]
            [clojure.string :as str]
            [reagent.core :as r]
            [re-frame.core :as f]
            [reflet.db :as db]))

(defonce tap-fn
  nil)

(defonce trace-index
  (r/atom {}))

(def queue-size
  "Number of events to tap per query. This should eventually be
  dynamic."
  50)

(defn qonj
  [q n x]
  (cond
    (nil? q)        #queue [x]
    (= n (count q)) (conj (pop q) x)
    :else           (conj q x)))

(defn- trace-event?
  [event]
  (let [id (first event)]
    (or (= id ::trace-cleanup)
        (let [ns (namespace id)]
          (not (str/starts-with? ns "reflet.debug"))))))

(defn trace?
  [event]
  (and tap-fn (trace-event? event)))

(defn- debug-trace-before
  [{{event :event} :coeffects
    :as            context}]
  (if (trace? event)
    (->> (update @trace-index ::t inc)
         (assoc-in context [:coeffects :db ::trace]))
    context))

(defn- commit-trace!
  [index event {t ::t :as trace}]
  (letfn [(rf [m q]
            (->> {:t     t
                  :event event}
                 (update m q qonj queue-size)))

          (f [m refs]
            (reduce rf m refs))]
    (when trace
      (->> index
           (::db/touched-entities)
           (update trace ::e->event f)
           (reset! trace-index)))))

(defn- debug-trace-after
  [{{event :event}   :coeffects
    {{index ::db/index
      trace ::trace
      :as   db} :db} :effects
    :as              context}]
  (if (and db (trace? event))
    (do (commit-trace! index event trace)
        (update-in context [:effects :db] dissoc ::trace))
    context))

(def debug-trace
  (f/->interceptor
   :id ::debug-trace
   :before debug-trace-before
   :after debug-trace-after))

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

(f/reg-event-db ::trace-cleanup
  ;; Only debug event specifically enabled in `trace-event?`
  [debug-trace]
  (fn [db [_ ref]]
    (-> db
        (update-in [::trace ::e->event] dissoc ref)
        (update-in [::trace ::fsm->transition] dissoc ref))))

(f/reg-sub ::taps
  (fn [db _]
    (get db ::taps)))

(f/reg-sub ::e->events
  (constantly (r/cursor trace-index [::e->event]))
  (fn [trace [_ ref]]
    (-> trace
        (get ref)
        (reverse)
        (not-empty))))

(f/reg-sub ::fsm->transitions
  (constantly (r/cursor trace-index [::fsm->transition]))
  (fn [trace [_ ref]]
    (->> (get trace ref)
         (group-by (juxt :t :event))
         (sort-by ffirst #(compare %2 %1))
         (not-empty))))

