(ns reflet.debug
  (:require [cinch.core :as util]
            [clojure.string :as str]
            [reagent.core :as r]
            [re-frame.core :as f]
            [reflet.db :as db]))

(defonce ^:dynamic *debug*
  nil)

(def queue-size
  "Number of events to tap per query. This should eventually be
  dynamic."
  50)

(defn- qonj
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
  [{touched ::touched-queries
    :as     index} event]
  (if touched
    (letfn [(rf [m q] (update m q qonj queue-size event))
            (f [m] (reduce rf m touched))]
      (update index ::q->event f))
    index))

(defn- debug-tap-after
  [{{event :event} :coeffects
    {db :db}       :effects
    :as            context}]
  (if (and *debug* db (domain-event? event))
    (update-in context
               [:effects :db ::index]
               update-debug-index
               event)
    context))

(f/reg-event-db ::tap
  ;; ::d/tap must happen after the ::d/untap of the previous react
  ;; lifecycle. To guarantee this, ::d/tap must be invoked in either
  ;; the `:ref` callback, or the `:component-did-mount` phase of the
  ;; component lifecycle. Must not dispatch in a `with-let`, where it
  ;; will happen during the first render.
  (fn [db [_ tap]]
    (let [id (find tap :debug/id)]
      (-> db
          (assoc-in [::taps id] tap)
          (db/mergen tap)))))

(f/reg-event-fx ::untap
  (fn [{db :db} [_ ref]]
    {:db (-> db
             (update ::taps dissoc ref)
             (db/dissocn ref))}))

(f/reg-sub ::taps
  (fn [db _]
    (get db ::taps)))

(def debug-tap-events
  (f/->interceptor
   :id ::debug-tap
   :after debug-tap-after))
