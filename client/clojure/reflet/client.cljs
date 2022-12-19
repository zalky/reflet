(ns reflet.client
  "Stand-alone client example, and testing."
  (:require [re-frame.core :as f*]
            [reagent.dom :as dom]
            [reflet.client.boot :as boot]
            [reflet.client.ui :as ui]
            [reflet.core :as f]

            ;; Require for use.
            [reflet.log]))

(defn render!
  []
  (f*/clear-subscription-cache!)
  (some->> "container"
           (.getElementById js/document)
           (dom/render [ui/app])))

(defn config!
  []
  (f/disp-sync [::f/config])
  (f/disp-sync [::boot/init-tracks]))

(defn init!
  []
  (config!)
  (render!))
