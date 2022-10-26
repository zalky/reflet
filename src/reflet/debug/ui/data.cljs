(ns reflet.debug.ui.data
  (:require [reflet.core :as f]
            [reflet.debug.ui.impl :as impl]))

(defn- ref?
  "Returns true if x is an entity reference."
  [x]
  (and (vector? x)
       (= (count x) 2)
       (keyword? (first x))))

(defmulti debug-value
  (fn [x]
    (cond
      (ref? x)    ::ref
      (string? x) ::string
      (map? x)    ::map
      :else       (type x))))

(defmethod debug-value ::ref
  [[attr value :as ref]]
  [:div {:class    "reflet-ref"
         :on-click #(f/disp [::impl/open-ref ref])}
   "@" (if (uuid? value)
         (subs (str value) 0 8)
         (str value))])

(defmethod debug-value ::string
  [s]
  [:div {:class "reflet-string"}
   (str \" s \")])

(defmethod debug-value ::map
  [m]
  [:div {:class "reflet-map"}
   [:div {:class "reflet-map-data"}
    (map-indexed
     (fn [i [k ref]]
       (when (not= k ::f/debug-id)
         [:div {:key i}
          [debug-value k]
          [debug-value ref]]))
     m)]])

(defmethod debug-value Keyword
  [k]
  [:div {:class "reflet-keyword"}
   (if-let [ns (namespace k)]
     [:<>
      [:span (str ":" ns "/")]
      [:span (name k)]]
     [:span (str k)])])

(defmethod debug-value :default
  [x]
  [:div (str x)])
