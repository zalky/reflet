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
  (fn [db [_ {id  :debug/id
              :as props}]]
    {:pre [id]}
    (-> db
        (assoc-in [::taps id] props)
        (db/mergen props))))

(f/reg-event-db ::untap
  (fn [db [_ ref]]
    (update db ::taps dissoc ref)))

(f/reg-sub ::taps
  (fn [db _]
    (get db ::taps)))

(def debug-tap-events
  (f/->interceptor
   :id ::debug-tap
   :after debug-tap-after))
