(ns reflet.client
  "Stand-alone client example, and testing."
  (:require [re-frame.core :as f*]
            [reagent.dom.client :as dom]
            [reflet.client.boot :as boot]
            [reflet.client.ui :as ui]
            [reflet.core :as f]))

(defonce root
  (dom/create-root (.getElementById js/document "container")))

(defn render!
  []
  (f*/clear-subscription-cache!)
  (dom/render root [(fn [] ui/app)]))

(defn config!
  []
  (f/disp-sync [::f/config])
  (f/disp-sync [::boot/init-data]))

(defn init!
  []
  (config!)
  (render!))
