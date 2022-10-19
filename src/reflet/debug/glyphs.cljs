(ns reflet.debug.glyphs
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
   [:svg {:view-box "0 0 10 6"}
    [:path (stroke {:d "M 9,1 Q 4,1 4,6"})]]
   [:div]
   [:svg {:height                "10"
          :view-box              "0 0 10 20"
          :preserve-aspect-ratio "none"}
    [:path (stroke {:d "M 4,0 Q 4,7.5 1,10 Q 4,12.5 4,20"})]]
   [:div]
   [:svg {:view-box "0 0 10 6"}
    [:path (stroke {:d "M 4,0 Q 4,5 9,5"})]]])

(defn mark-icon
  [& [{:keys [group]}]]
  [:svg {:view-box "0 0 20 30"}
   [:circle {:cx    10
             :cy    10
             :r     9
             :style {:fill "currentColor"}}]
   (when group
     [:<>
      [:path
       (stroke
        {:d     "M 3,20 A 11,11 0 0 0 17,20"
         :style {:stroke-width "1.5px"}})]
      [:path
       (stroke
        {:d     "M 3,25 A 13,13 0 0 0 17,25"
         :style {:stroke-width "1.5px"}})]])])
