(ns reflet.client
  "Stand-alone client example, and testing."
  (:require [reagent.dom :as dom]
            [reflet.client.boot :as boot]
            [reflet.client.ui :as ui]
            [reflet.core :as f]

            ;; Require for use.
            [reflet.log]))

(defn ^:dev/after-load render!
  []
  (f/clear-subscription-cache!)
  (some->> "container"
           (.getElementById js/document)
           (dom/render [ui/app])))

(defn init!
  []
  (f/dispatch-sync [::boot/boot])
  (render!))
