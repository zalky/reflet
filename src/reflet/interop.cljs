(ns reflet.interop
  "Provides a DB for stateful JS objects and DOM nodes. Objects are
  indexed by an entity reference obtained through
  `reflet.core/with-ref`.

  The `with-ref` macro ensures proper cleanup of objects when React
  component unmounts.  It is important that entity refs are never
  created outside of the `with-ref` macro, as that would cause leaks."
  (:require [cljs.spec.alpha :as s]
            [re-frame.core :as f]
            [reagent.ratom :as r]))

(defonce db
  (r/atom {}))

(s/def ::ref
  (s/tuple qualified-keyword? uuid?))

(defn grab
  "The preferred function for getting objects in effect handlers.
  Accepts the corresponding object DB as first argument, ref as
  second. In an event handler, the DB is typically obtained by
  injecting the coeffect: (f/inject-cofx ::db)"
  [db ref]
  (:obj (get db ref)))

(defn update!
  "Semantics like `clojure.core/update`, mutates the object in db by
  running the function with existing object as parameter."
  [ref f & args]
  (apply swap! db update-in [ref :obj] f args))

(f/reg-sub ::grab
  ;; Subscription for use in components and other subscriptions
  ;; Usage `(f/subscribe [::grab node-ref])`
  (constantly db)
  (fn [db [_ ref]]
    (grab db ref)))

(defn node-mounted?
  "If a DOM node is stored in this DB with the provided ref, it has been mounted"
  [ref]
  (boolean (get @db ref)))

(defn reg
  "Stores the interop oobject in the object DB. Optionally accepts a
  spec with a single key, `:destroy`. If a destroy fn is provided,
  that function will be run at teardown with the obj as argument."
  ([ref obj]
   (reg ref obj nil))
  ([ref obj opts]
   (->> opts
        (merge {:obj obj})
        (swap! db assoc ref))))

(f/reg-fx ::cleanup
  ;; Called from `with-ref` cleanup to remove references belonging to
  ;; an unmounted React comp
  (fn [refs]
    (doseq [ref refs]
      (when-let [{:keys [destroy obj]} (get @db ref)]
        (when destroy
          (destroy obj))
        (swap! db dissoc ref)))))

(f/reg-cofx :interop-db
  ;; Provides the JS db as a co-effect to event handlers
  (fn [cofx]
    (assoc cofx :interop-db @db)))

(defn node!
  "Returns a react callback for initializing a node ref and putting it
  into the dom node db.

  Signal is only initialized once. Can optionally flush reagent ratom
  queue for immediate computation, or provide a callback for when the
  component is mounted. This callback should only be used in the
  special case where you don't have access to the component's
  lifecycle."
  [ref & {:keys [flush cb]}]
  (fn [node]
    (when-not (node-mounted? ref)
      (reg ref node)
      (when cb (cb node))
      (when flush (r/flush!)))))

(defn id
  "Node ids cannot start with a number."
  [ref]
  (str "id-" (second ref)))
