(ns reflet.client.ui
  (:require [reagent.core :as r]
            [reflet.client.ui.desc :as desc]
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
   [:a {:on-click #(f/disp [::impl/set-view ::player])}
    "Player"]
   [:a {:on-click #(f/disp [::impl/set-view ::workflow])}
    "Workflow"]
   [:a {:on-click #(f/disp [::impl/set-view ::descriptions])}
    "Descriptions"]])

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
        ::workflow     [work/workflow props]
        ::descriptions [desc/descriptions props]
        [player/player props])]]))
