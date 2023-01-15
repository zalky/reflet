(ns reflet.interop
  "Provides a DB for JS objects and DOM nodes.

  Items are indexed by entity references obtained through the
  `reflet.core/with-ref` macro, and cleaned up when their React
  component unmounts. Do not use refs that were not created by
  `with-ref` unless you are prepared to clean them up yourself."
  (:require [cljs.spec.alpha :as s]
            [re-frame.core :as f]
            [re-frame.db :as rdb]
            [reagent.ratom :as r]
            [reflet.db :as db]
            [reflet.db.normalize :as norm]))

(defonce db
  (r/atom {}))

(s/def ::ref
  (s/tuple qualified-keyword? uuid?))

(f/reg-sub ::grab
  ;; Subscription for accessing objects in reactive contexts. You
  ;; generally want to avoid this except in specific circumstances,
  ;; like when you need to perform reactive style computations based
  ;; on properties of the underlying DOM node.
  ;; are
  (constantly db)
  (fn [db [_ ref]]
    (:obj (get db ref))))

(defn grab
  "The preferred function for accessing objects in non-reactive
  contexts. We do not bother with co-effects. Operations on the DOM or
  on JS objects are inherently mutable, and not pure."
  [ref]
  (if (r/reactive?)
    (-> "Do not use reflet.interop/grab in reactive context, prefer ::grab sub"
        (ex-info {:ref ref})
        (throw))
    (:obj (get (.-state db) ref))))

(defn update!
  "Semantics like `clojure.core/update`, mutates the object in db by
  running the function with existing object as parameter."
  [ref f & args]
  (apply swap! db update-in [ref :obj] f args))

(defn reg
  "Stores the interop object in the object DB. Optionally accepts a
  spec with a single key, `:destroy`. If a destroy fn is provided,
  that function will be run at tear-down with the obj as argument."
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
    (doseq [ref (->> @rdb/app-db
                     (::db/id-attrs)
                     (norm/to-many refs))]
      (when-let [{:keys [destroy obj]} (get @db ref)]
        (when destroy (destroy obj))
        (swap! db dissoc ref)))))

(defn el!
  "Returns a react callback for initializing a DOM element ref and
  putting it into the interop db. Signal is only initialized once.

  Options include:

  :mount
            Callback function is called after DOM element is
            mounted. Accepts the DOM element as it's only argument.

  :flush
            Flushes the reagent ratom queue for immediate
            computation"
  [ref & {:keys [flush mount]}]
  (when-not (:obj (get (.-state db) ref))
    (fn [el]
      (when el
        (reg ref el)
        (when mount (mount el))
        (when flush (r/flush!))))))

(defn id
  "DOM element ids cannot start with a number."
  [ref]
  (str "id-" (second ref)))
