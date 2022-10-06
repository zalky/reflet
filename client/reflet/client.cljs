(ns reflet.client
  "Stand-alone client example, and testing."
  (:require [reactstrap-cljs.core :as b]
            [reagent.dom :as dom]
            [reflet.core :as f]
            [reflet.db :as db]

            ;; Require for use.
            [reflet.log]))

(f/reg-event-fx ::init
  (fn [_ _]
    {:db (db/new-db)}))

(defn root
  []
  [:div
   [:div {:class "welcome mb-2"}
    "Hello, world!"]
   [b/button {:color :primary}
    "Submit"]])

(defn render!
  []
  (f/clear-subscription-cache!)
  (some->> "container"
           (.getElementById js/document)
           (dom/render [root])))

(defn init!
  []
  (f/dispatch-sync [::init])
  (render!))

