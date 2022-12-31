(ns reflet.interceptors
  (:require [cinch.core :as util]
            [re-frame.cofx :as cofx]
            [re-frame.core :as f]
            [re-frame.events :as events]
            [re-frame.fx :as fx]
            [re-frame.std-interceptors :as itor]))

(defonce db
  (atom {}))

(defn reg-global-interceptor
  ([id interceptor]
   (reg-global-interceptor ::default-group id interceptor))
  ([group id interceptor]
   (swap! db assoc-in [group id] interceptor)))

(defn clear-global-interceptor
  ([id]
   (clear-global-interceptor ::default-group id))
  ([group id]
   (swap! db update group dissoc id)))

(defn get-global-interceptor
  ([id]
   (get-global-interceptor ::default-group id))
  ([group id]
   (get-in @db [group id])))

(defn global-interceptor-registered?
  ([id]
   (global-interceptor-registered? ::default-group id))
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
  retaining some imperative control."
  ([]
   (add-global-interceptors ::default-group))
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

(defn with-cycle-id
  [id]
  (vary-meta id assoc ::cycle-id (random-uuid)))

(defn same-cycle?
  "Cycle ids can be used to ensure that interceptor operations from
  different runtime lifecycles do not clobber each other. For example,
  you can use this function to check if the lifecycle handler from
  BEFORE a hot reload is attempting to clear an interceptor that was
  registered AFTER the hot reload."
  [id]
  (->> (vals @db)
       (some #(find % id))
       (first)
       (cycle-id)
       (= (cycle-id id))))

(defn- all-interceptors
  "Reflet implementation interceptors must happen before Re-frame
  globals. User interceptors happen after everything else."
  [impl-itors user-itors]
  [cofx/inject-db
   fx/do-fx
   impl-itors
   itor/inject-global-interceptors
   user-itors])

(defn reg-event
  "Registers a pre-wrapped interceptor handler."
  ([id impl-itors handler]
   (reg-event id impl-itors nil handler))
  ([id impl-itors user-itors handler]
   (->> handler
        (conj (all-interceptors
               impl-itors
               user-itors))
        (events/register id))))

(defn reg-event-db-impl
  ([id impl-itors handler]
   (reg-event-db-impl id impl-itors nil handler))
  ([id impl-itors user-itors handler]
   (reg-event id impl-itors user-itors
     (itor/db-handler->interceptor handler))))

(defn reg-event-fx-impl
  ([id impl-itors handler]
   (reg-event-fx-impl id impl-itors nil handler))
  ([id impl-itors user-itors handler]
   (reg-event id impl-itors user-itors
     (itor/fx-handler->interceptor handler))))

(defn reg-event-ctx-impl
  ([id impl-itors handler]
   (reg-event-ctx-impl id impl-itors nil handler))
  ([id impl-itors user-itors handler]
   (reg-event id impl-itors user-itors
     (itor/ctx-handler->interceptor handler))))
