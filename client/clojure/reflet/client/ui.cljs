(ns reflet.client.ui
  (:require [reactstrap-cljs.core :as b]
            [reagent.core :as r]
            [reflet.client.ui.form :as form]
            [reflet.client.ui.impl :as impl]
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
    [b/nav-link {:on-click #(f/disp [::impl/set-view ::player])}
     "Player"]]
   [b/nav-item
    [b/nav-link {:on-click #(f/disp [::impl/set-view ::form])}
     "Form"]]])

(defn app
  []
  (f/with-ref {:cmp/uuid [app/self]
               :in       props}
    [:div {:class "app"}
     [:div
      [header]
      [menu props]]
     [:div {:class "view"}
      (case @(f/sub [::impl/view])
        ::form [form/form props]
        [player/player props])]]))
