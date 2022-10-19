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
