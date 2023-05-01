(ns reflet.client.ui.desc
  (:require [cljs.pprint :as pprint]
            [reflet.client.ui.desc.impl :as impl]
            [reflet.core :as f]))

(defn- select
  [self c]
  (fn [_]
    (f/disp [::impl/select-context self c])))

(defn- description
  [{:desc/keys [self]}]
  (let [context @(f/sub [::impl/selected-context self])
        data    @(f/desc [context :desc/event-list])]
    [:div {:class "description"}
     [:pre [:code (str "@(f/desc [" context " :desc/event-list])")]]
     [:pre [:code "=>"]]
     [:pre [:code (with-out-str (pprint/pprint data))]]]))

(defn descriptions
  [props]
  (f/with-ref {:cmp/uuid [desc/self]
               :in       props}
    [:div {:class "descriptions"}
     [:div
      [:a {:on-click (select self ::impl/condensed)} "Condensed"]
      [:span "/"]
      [:a {:on-click (select self ::impl/detailed)} "Detailed"]]
     [description props]]))
