(ns reflet.debug.glyphs
  (:require [cinch.core :as util]))

(def path-opts
  {:vector-effect     "non-scaling-stroke"
   :fill              "transparent"
   :stroke            "currentColor"
   :stroke-width      "1.5px"
   :stroke-linecap    "round"
   :stroke-linejoin   "round"
   :stroke-miterlimit "1.5"})

(defn stroke
  [el]
  (util/merge-deep path-opts el))

(defn glyph-props
  [props]
  (update props :class #(conj (util/seqify %) "reflet-glyph")))

(defn mark-icon
  [& [{:keys [group]
       :as   props}]]
  [:div (glyph-props props)
   [:svg {:view-box "0 0 20 30"}
    [:circle {:cx   10
              :cy   10
              :r    9
              :fill "currentColor"}]
    (when group
      [:<>
       [:path (stroke {:d "M 3,20 A 11,11 0 0 0 17,20"})]
       [:path (stroke {:d "M 3,25 A 13,13 0 0 0 17,25"})]])]])

(defn x
  [& [props]]
  [:div (glyph-props props)
   [:svg {:view-box "0 0 10 10"}
    [:path (stroke {:d "M 1,1 L 9,9 M 9,1 L 1,9"})]]])

(defn handle
  [& [props]]
  [:div (glyph-props props)
   [:svg {:view-box "0 0 10 10"}
    [:path (stroke {:d "M 8,2 A 7,7 0 0 1 2,8 M 6,2 A 5,5 0 0 1 2,6"})]]])

(defn coll-expander
  [& [props]]
  [:div (glyph-props props)
   [:svg {:view-box "0 0 20 10"}
    [:path (stroke {:d            "M 2,2 L 10,8 L 18,2"
                    :stroke-width "1px"})]]])

(defn back
  [& [props]]
  [:div (glyph-props props)
   [:svg {:view-box "0 0 10 10"}
    [:path (stroke {:d "M 9,1 L 1,5 L 9,9"})]]])
