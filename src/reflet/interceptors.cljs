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

(defn cycle-id
  [id]
  (-> id
      (meta)
      (::cycle-id)))

(defn with-cycle-id
  [id]
  (-> id
      (meta)
      (::cycle-id)))

(defn new-cycle-id
  [id]
  (vary-meta id assoc ::cycle-id (random-uuid)))

(defn same-cycle?
  "Cycle ids can be used to ensure that interceptor operations from
  different runtime lifecycles do not clobber each other. For example,
  if interceptor stop handler from before a hot reload tries to clear
  an interceptor that was registered after the hot reload."
  [id]
  (let [cid (cycle-id id)]
    (-> @reg/kind->id->handler
        (get :global-interceptor)
        (find id)
        (first)
        (cycle-id)
        (= cid))))

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
