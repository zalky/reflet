(ns reflet.interceptors
  (:require [cinch.core :as util]
            [re-frame.core :as f]
            [re-frame.registrar :as reg]))

(defn reg-global-interceptor
  [id interceptor]
  (reg/register-handler :global-interceptor id interceptor))

(defn clear-global-interceptor
  [id]
  (swap! reg/kind->id->handler
         update-in
         [:global-interceptor]
         dissoc id))

(defn global-interceptor-registered?
  [id]
  (reg/get-handler :global-interceptor id))

(def add-global-interceptors
  "Adds global interceptors to the beginning of queue."
  (letfn [(cut-in-queue [queue xs]
            (into #queue [] (concat xs queue)))
          (add-global-interceptors* [context]
            (let [globals (vals (reg/get-handler :global-interceptor))]
              (update context :queue cut-in-queue globals)))]
    (f/->interceptor
     :id :add-global-interceptors
     :before add-global-interceptors*)))

(defn- to-many
  [x]
  (cond
    (vector? (first x))  x
    (keyword? (first x)) [x]))

(f/reg-fx ::reg-global-interceptor
  (fn [xs]
    (doseq [[id interceptor] (to-many xs)]
      (reg-global-interceptor id interceptor))))

(f/reg-fx ::clear-global-interceptor
  (fn [xs]
    (doseq [id (util/seqify xs)]
      (clear-global-interceptor id))))
