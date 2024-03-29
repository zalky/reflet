(ns reflet.debug.glyphs
  (:require [cinch.core :as util]))

(def path-opts
  {:vector-effect     "non-scaling-stroke"
   :fill              "none"
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
  [:div (glyph-props (dissoc props :group))
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
    [:path (stroke {:d "M 8,4 L 8,8 4,8"})]]])

(defn coll-expander
  [& [props]]
  [:div (glyph-props props)
   [:svg {:view-box "0 0 20 10"}
    [:path (stroke {:d            "M 2,2 L 10,8 L 18,2"
                    :stroke-width "1px"})]]])

(defn fsm
  [& [props]]
  [:div (glyph-props props)
   [:svg {:view-box "0 0 16 32"}
    [:path (stroke {:d            "M 15,8 L 6,8 L 6,21 L 6,25 15,25 M 12,22 L 15,25 12,28"
                    :stroke-width "1px"})]]])

(defn menu
  [& [props]]
  [:div (glyph-props props)
   [:svg {:view-box "0 0 10 10"}
    [:path (stroke {:d "M 1,1 L 9,1 M 1,5 L 9,5 M 1,9 L 9,9"})]]])

(defn back
  [& [props]]
  [:div (glyph-props props)
   [:svg {:view-box "0 0 8 10"}
    [:path (stroke {:d "M 5,1 L 1,5 L 5,9 Z"})]]])

(defn close-all
  [& [props]]
  [:div (glyph-props props)
   [:svg {:view-box "0 0 20 15"}
    [:path (stroke {:d "M 1,1 L 16,1 16,11 1,11 Z M 19,3 L 19,14 3,14 M 9,4 L 13,8 M 9,8 L 13,4"})]]])
