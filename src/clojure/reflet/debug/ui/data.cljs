(ns reflet.debug.ui.data
  (:require [reflet.core :as f]
            [reflet.db :as db]
            [reflet.db.normalize :as norm]
            [reflet.debug.glyphs :as g]
            [reflet.debug.ui.data.impl :as impl]
            [reflet.debug.ui.impl :as ui]))

(defn- ref?
  "Returns true if x is an entity reference."
  [x]
  (norm/ref? x @(f/sub [::db/id-attrs])))

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

(defn- pos
  [e]
  {:x (.-clientX e)
   :y (.-clientY e)})

(defn on-click
  [ref]
  (fn [e]
    (f/disp [::ui/open-ref-panel ref])))

(defn on-context-click
  [value]
  (fn [e]
    (.preventDefault e)
    (f/disp [::ui/open-context value (pos e)])))

(defmethod map-entry :default
  [i k v]
  [:div {:key i}
   [value k]
   [value v]])

(defmethod value :default
  [x]
  [:div {:on-context-menu (on-context-click x)}
   (str x)])

(defmethod value nil
  [_]
  [:div {:class "reflet-nil"} "nil"])

(defmethod value js/Number
  [n]
  [:div {:class           "reflet-number"
         :on-context-menu (on-context-click n)}
   n])

(defmethod value js/String
  [s]
  [:div {:class           "reflet-string"
         :on-context-menu (on-context-click s)}
   [:span (str \" s \")]])

(defn value-ref
  [ref & {:keys [on-click]}]
  (let [[attr value] ref]
    [:div {:class           "reflet-ref"
           :on-click        on-click
           :on-context-menu (on-context-click ref)}
     (namespace attr) "@"
     (if (uuid? value)
       (subs (str value) 0 8)
       (str value))]))

(defmethod value ::ref
  [ref]
  (value-ref ref :on-click (on-click ref) ))

(defmethod value Keyword
  [k]
  [:div {:class           "reflet-keyword"
         :on-context-menu (on-context-click k)}
   [:span ":"]
   [:span (apply str (rest (str k)))]])

(defmethod value ::map
  [m]
  [:div {:class "reflet-map"}
   [:div {:class "reflet-map-data"}
    (doall
     (map-indexed
      (fn [i [k v]]
        ^{:key i} [map-entry i k v])
      m))]])

(defmethod value ::coll
  [coll]
  [:div {:class (cond
                  (vector? coll) "reflet-vec"
                  (list? coll)   "reflet-list"
                  (set? coll)    "reflet-set"
                  :else          nil)}
   [:div {:class "reflet-coll-data"}
    (doall
     (map-indexed
      (fn [i x]
        ^{:key i} [value x])
      coll))]])

(defn- inline-coll
  [coll]
  [:div (doall
         (map-indexed
          (fn [i x]
            ^{:key i} [value x])
          coll))])

(defn- expander
  [{:expander/keys [self]} v]
  (let [e      (g/coll-expander)
        toggle #(f/disp [::impl/toggle-expand self])]
    [:div {:on-click toggle
           :class    "reflet-coll-expander"}
     (cond
       (vector? v) [:<> [:span "["] (inline-coll v) e [:span "]"]]
       (map? v)    [:<> [:span "{"] (inline-coll (flatten (seq v))) e [:span "}"]]
       (list? v)   [:<> [:span "("] (inline-coll v) e [:span ")"]]
       (set? v)    [:<> [:span "#{"] (inline-coll v) e [:span "}"]])]))

(defmethod map-entry ::coll
  [i k v]
  (f/with-ref* {:cmp/uuid [expander/self]
                :in       props}
    (let [expand? (f/sub [::impl/expand? self])]
      [:<> {:key i}
       [:div {:class (when @expand? "reflet-coll-expand")}
        [value k]
        [expander props v]]
       (when @expand?
         [:div {:class "reflet-coll-expanded"}
          [value v]])])))
