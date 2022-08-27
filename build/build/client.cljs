(ns build.client
  "Stand-alone client example, and testing."
  (:require [reagent.dom :as dom]
            [reflet.core :as f]))

(defn root
  []
  [:div "Hello, world!"])

(defn render!
  []
  (f/clear-subscription-cache!)
  (some->> "container"
           (.getElementById js/document)
           (dom/render [root])))

(defn init!
  []
  (render!))
