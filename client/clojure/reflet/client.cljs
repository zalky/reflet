(ns reflet.client
  "Stand-alone client example, and testing."
  (:require [re-frame.core :as f*]
            [reagent.dom :as dom]
            [reflet.client.boot :as boot]
            [reflet.client.ui :as ui]
            [reflet.core :as f]))

(def config-desc
  {})

(defn render!
  "Also called on every hot-reload."
  []
  (f*/clear-subscription-cache!)
  (f/disp-sync [::f/config-desc config-desc])
  (some->> "container"
           (.getElementById js/document)
           (dom/render [ui/app])))

(defn config!
  []
  (f/disp-sync [::f/config])
  (f/disp-sync [::boot/init-data]))

(defn init!
  "Called once on boot."
  []
  (config!)
  (render!))
