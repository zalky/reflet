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

(defn- debug-tag?
  [id]
  (-> (str id)
      (str/starts-with? ":reflet.debug")))

(defn- domain-event?
  [event]
  (-> event
      (first)
      (debug-tag?)
      (not)))

(defn trace?
  [event]
  (and tap-fn (domain-event? event)))

(defn- debug-tap-before
  [{{event :event} :coeffects
    :as            context}]
  (if (trace? event)
    (->> inc
         (update @trace-index ::t)
         (assoc-in context [:coeffects :db ::trace]))
    context))

(defn- update-trace!
  [index event {t ::t :as trace}]
  (letfn [(rf [m q]
            (->> {:t     t
                  :event event}
                 (update m q qonj queue-size)))

          (f [m refs]
            (reduce rf m refs))]
    (when trace
      (some->> index
               (::db/touched-entities)
               (update trace ::e->event f)
               (reset! trace-index)))))

(defn- debug-tap-after
  [{{event :event}   :coeffects
    {{index ::db/index
      trace ::trace
      :as   db} :db} :effects
    :as              context}]
  (if (and db (trace? event))
    (do (update-trace! index event trace)
        (->> (dissoc db ::trace)
             (assoc-in context [:effects :db])))
    context))

(def debug-tap-events
  (f/->interceptor
   :id ::debug-tap
   :before debug-tap-before
   :after debug-tap-after))

(f/reg-event-db ::tap
  ;; ::d/tap must happen after the ::d/untap of the previous react
  ;; lifecycle. To guarantee this, ::d/tap must be invoked in either
  ;; the `:ref` callback, or the `:component-did-mount` phase of the
  ;; component lifecycle. Must not dispatch in a `with-let`, where it
  ;; will happen during the first render.
  (fn [db [_ ref tap]]
    (-> db
        (update-in [::taps ref] merge tap)
        (db/mergen tap))))

(f/reg-event-fx ::untap
  (fn [{db :db} [_ ref]]
    {:db (-> db
             (update ::taps dissoc ref)
             (db/dissocn ref))}))

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

