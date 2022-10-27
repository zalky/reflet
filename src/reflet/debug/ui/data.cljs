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
      (map? x)    ::map
      :else       (type x))))

(defmethod debug-value nil
  [_]
  [:div {:class "reflet-nil"} "nil"])

(defmethod debug-value js/Number
  [n]
  [:div {:class "reflet-number"} n])

(defmethod debug-value ::ref
  [[attr value :as ref]]
  [:div {:class    "reflet-ref"
         :on-click #(f/disp [::impl/open-ref ref])}
   "@" (if (uuid? value)
         (subs (str value) 0 8)
         (str value))])

(defmethod debug-value js/String
  [s]
  [:div {:class "reflet-string"}
   [:span (str \" s \")]])

(defmethod debug-value ::map
  [m]
  [:div {:class "reflet-map"}
   [:div {:class "reflet-map-data"}
    (map-indexed
     (fn [i [k ref]]
       [:div {:key i}
        [debug-value k]
        [debug-value ref]])
     m)]])

(defmethod debug-value Keyword
  [k]
  [:div {:class "reflet-keyword"}
   [:span ":"]
   (when-let [ns (namespace k)]
     [:<>
      [:span ns]
      [:span "/"]])
   [:span (name k)]])

(defmethod debug-value :default
  [x]
  [:div (str x)])
