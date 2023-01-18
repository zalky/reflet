(ns reflet.client.ui
  (:require [reagent.core :as r]
            [reflet.client.ui.impl :as impl]
            [reflet.client.ui.player :as player]
            [reflet.client.ui.workflow :as work]
            [reflet.core :as f]))

(defn header
  [_]
  [:div {:class "header"}
   "Reflet"])

(defn menu
  [_]
  [:div {:class "menu"}
   [:div {:on-click #(f/disp [::impl/set-view ::player])}
    "Player"]
   [:div {:on-click #(f/disp [::impl/set-view ::workflow])}
    "Workflow"]])

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
        ::workflow [work/workflow props]
        [player/player props])]]))
