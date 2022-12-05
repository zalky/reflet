(ns reflet.client.ui
  (:require [reactstrap-cljs.core :as b]
            [reagent.core :as r]
            [reflet.client.ui.player :as player]
            [reflet.core :as f]))

(defn header
  [_]
  [:div {:class "header"}
   "Reflet"])

(defn menu
  [_]
  [b/nav {:class "menu"}
   [b/nav-item
    [b/nav-link "Player"]]
   [b/nav-item
    [b/nav-link "Form"]]])

(defn app
  []
  (f/with-ref {:cmp/uuid [app/self]
               :in       props}
    [:div {:class "app"}
     [:div
      [header]
      [menu props]]
     [:div {:class "view"}
      [player/player props]]]))
