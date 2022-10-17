(ns reflet.debug
  (:require [clojure.string :as str]
            [reagent.core :as r]
            [re-frame.core :as f]))

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

(def debug-tap-events
  (f/->interceptor
   :id ::debug-tap
   :after debug-tap-after))
