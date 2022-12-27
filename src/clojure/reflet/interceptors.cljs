(ns reflet.interceptors
  (:require [cinch.core :as util]
            [re-frame.core :as f]
            [re-frame.registrar :as reg]))

(defonce db
  (atom {}))

(defn reg-global-interceptor
  ([id interceptor]
   (reg-global-interceptor ::global-interceptor id interceptor))
  ([group id interceptor]
   (swap! db assoc-in [group id] interceptor)))

(defn clear-global-interceptor
  ([id]
   (clear-global-interceptor ::global-interceptor id))
  ([group id]
   (swap! db update group dissoc id)))

(defn get-global-interceptor
  ([id]
   (get-global-interceptor ::global-interceptor id))
  ([group id]
   (get-in @db [group id])))

(defn global-interceptor-registered?
  ([id]
   (global-interceptor-registered? ::global-interceptor id))
  ([group id]
   (boolean (get-global-interceptor group id))))

(defn- group-id
  [group]
  (->> (name group)
       (str (namespace group) "-")
       (keyword "add-global-interceptors")))

(defn add-global-interceptors
  "Adds a global interceptor group to the beginning of the current
  queue. The Re-frame implementation allows for ordered dependencies
  between global interceptors. But the order is based on the order
  they are registered, which is runtime dependent. This introduces
  considerable imperative complexity to the interceptor chain. The
  Reflet implementation tries to have it both ways. With this function
  you can define groups of global interceptors, and order the groups
  statically in code within the interceptor chain. However, because
  the membership of the group is dynamically determined at runtime,
  the order within each group is left undefined. For example, you
  could statically declare ALL FSM interceptors should happen before
  ALL input validation interceptors, but have no defined order within
  either group. This results in a more robust architecture, while
  retaining some imperative control as well."
  ([]
   (add-global-interceptors ::global-interceptor))
  ([group]
   (letfn [(cut-in-queue [queue xs]
             (into #queue [] (concat xs queue)))
           (add-global-interceptors* [context]
             (let [globals (vals (get @db group))]
               (update context :queue cut-in-queue globals)))]
     (f/->interceptor
      :id (group-id group)
      :before add-global-interceptors*))))

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

(defn cycle-id
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
  if the lifecycle handler from before a hot reload tries to clear an
  interceptor that was registered after the hot reload."
  [id]
  (->> (vals @db)
       (some #(find % id))
       (first)
       (cycle-id)
       (= (cycle-id id))))
