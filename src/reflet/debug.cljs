(ns reflet.debug
  (:require [cinch.core :as util]
            [clojure.string :as str]
            [reagent.core :as r]
            [re-frame.core :as f]
            [reflet.db :as db]))

(defonce tap-fn
  nil)

(defonce trace
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
  (->> (str id)
       (str/starts-with? ":reflet.debug")))

(defn- domain-event?
  [event]
  (-> event
      (first)
      (debug-tag?)
      (not)))

(defn- update-debug-index
  [{touched-q ::db/touched-queries
    touched-e ::db/touched-entities
    tick      ::db/tick
    :as       index} event]
  (if (or touched-q touched-e)
    (letfn [(rf [m q]
              (->> {:t     tick
                    :event event}
                   (update m q qonj queue-size)))

            (f [m refs]
              (reduce rf m refs))]
      (-> index
          (update ::db/q->event f touched-q)
          (update ::db/e->event f touched-e)))
    index))

(defn- debug-tap-after
  [{{event :event} :coeffects
    {db :db}       :effects
    :as            context}]
  (if (and tap-fn db (domain-event? event))
    (update-in context
               [:effects :db ::db/index]
               update-debug-index
               event)
    context))

(def debug-tap-events
  (f/->interceptor
   :id ::debug-tap
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
  (constantly (r/cursor db/query-index [::db/e->event]))
  (fn [index [_ ref]]
    (-> index
        (get ref)
        (reverse)
        (not-empty))))

(f/reg-sub ::fsm->transitions
  (constantly (r/cursor trace [::fsm->transition]))
  (fn [index [_ ref]]
    (-> index
        (get ref)
        (reverse)
        (not-empty))))

