(ns reflet.debugger.glyphs
  (:require [cinch.core :as util]))

(def path-opts
  {:vector-effect "non-scaling-stroke"
   :style         {:fill              "transparent"
                   :stroke            "currentColor"
                   :stroke-width      "1px"
                   :stroke-linecap    "round"
                   :stroke-linejoin   "round"
                   :stroke-miterlimit "1.5"}})

(defn stroke
  [el]
  (util/merge-deep path-opts el))

(defn brace
  []
  [:div {:class "debug-map-brace"}
   [:svg {:view-box "0 0 14 6"}
    [:path (stroke {:d "M 12,1 Q 6,1 6,6"})]]
   [:div]
   [:svg {:height                "10"
          :view-box              "0 0 14 20"
          :preserve-aspect-ratio "none"}
    [:path (stroke {:d "M 6,0 Q 6,7.5 2,10 Q 6,12.5 6,20"})]]
   [:div]
   [:svg {:view-box "0 0 14 6"}
    [:path (stroke {:d "M 6,0 Q 6,5 12,5"})]]])

(defn node-icon
  [& [{:keys [stack]}]]
  [:svg {:view-box "0 0 20 30"}
   [:circle {:cx    10
             :cy    10
             :r     9
             :style {:fill "currentColor"}}]
   (when stack
     [:<>
      [:path
       (stroke
        {:d     "M 3,20 A 11,11 0 0 0 17,20"
         :style {:stroke-width "2px"}})]
      [:path
       (stroke
        {:d     "M 3,25 A 13,13 0 0 0 17,25"
         :style {:stroke-width "2px"}})]])])
