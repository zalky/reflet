(ns reflet.debug.ui.data
  (:require [reflet.core :as f]
            [reflet.debug.glyphs :as g]
            [reflet.debug.ui.data.impl :as impl]
            [reflet.debug.ui.impl :as ui]))

(defn- ref?
  "Returns true if x is an entity reference."
  [x]
  (and (vector? x)
       (= (count x) 2)
       (keyword? (first x))))

(defmulti value
  (fn [x]
    (cond
      (ref? x)  ::ref
      (map? x)  ::map
      (coll? x) ::coll
      :else     (type x))))

(defmulti map-entry
  (fn [i k v]
    (if (and (coll? v)
             (not (ref? v)))
      ::coll
      :default)))

(defmethod map-entry :default
  [i k v]
  [:div {:key i}
   [value k]
   [value v]])

(defmethod value :default
  [x]
  [:div (str x)])

(defmethod value nil
  [_]
  [:div {:class "reflet-nil"} "nil"])

(defmethod value js/Number
  [n]
  [:div {:class "reflet-number"} n])

(defmethod value js/String
  [s]
  [:div {:class "reflet-string"}
   [:span (str \" s \")]])

(defmethod value ::ref
  [[attr value :as ref]]
  [:div {:class    "reflet-ref"
         :on-click #(f/disp [::ui/open-ref ref])}
   (namespace attr) "@"
   (if (uuid? value)
     (subs (str value) 0 8)
     (str value))])

(defmethod value Keyword
  [k]
  [:div {:class "reflet-keyword"}
   [:span ":"]
   (when-let [ns (namespace k)]
     [:<>
      [:span ns]
      [:span "/"]])
   [:span (name k)]])

(defmethod value ::map
  [m]
  [:div {:class "reflet-map"}
   [:div {:class "reflet-map-data"}
    (doall
     (map-indexed
      (fn [i [k v]]
        (map-entry i k v))
      m))]])

(defmethod value ::coll
  [coll]
  [:div
   (doall
    (map-indexed
     (fn [i x]
       [:<> {:key i}
        [value x]])
     coll))])

(defn- expander
  [{:expander/keys [self]} v]
  (let [e       (g/coll-expander)
        expand? (f/sub [::impl/expand? self])
        toggle  #(f/disp [::impl/toggle-expand self])]
    [:div {:on-click toggle
           :class    ["reflet-coll-expander"
                      (when @expand? "reflet-coll-expand")]}
     (cond
       (vector? v) [:<> [:span "["] e [:span "]"]]
       (map? v)    [:<> [:span "{"] e [:span "}"]]
       (list? v)   [:<> [:span "("] e [:span ")"]]
       (set? v)    [:<> [:span "#{"] e [:span "}"]])]))

(defn- expanded
  [{:expander/keys [self]} v]
  (let [expand? (f/sub [::impl/expand? self])]
    [:div {:class ["reflet-coll-expanded"
                   (when @expand? "reflet-coll-expand")
                   (cond
                     (vector? v) "reflet-vec"
                     (list? v)   "reflet-list"
                     (set? v)    "reflet-set"
                     :else       nil)]}
     [value v]]))

(defmethod map-entry ::coll
  [i k v]
  (f/with-ref* {:cmp/uuid [expander/self]
                :in       props}
    (let [expand? (f/sub [::impl/expand? self])]
      [:<> {:key i}
       [:div
        [value k]
        [expander props v]]
       (when @expand?
         [:div
          [expanded props v]])])))
