(ns reflet.debug.ui.data
  (:require [reflet.core :as f]
            [reflet.db :as db]
            [reflet.db.normalize :as norm]
            [reflet.debug.glyphs :as g]
            [reflet.debug.ui.data.impl :as impl]
            [reflet.debug.ui.impl :as ui]
            [reflet.interop :as i]))

(def uuid-condensed-chars
  "The number of characters to condense a UUID."
  8)

(defn- ref?
  "Returns true if x is an entity reference."
  [x]
  (norm/ref? x @(f/sub [::db/id-attrs])))

(defmulti value
  (fn [x & _]
    (cond
      (ref? x)  ::ref
      (map? x)  ::map
      (coll? x) ::coll
      :else     (type x))))

(defmulti map-entry
  (fn [i k v]
    (if (and (coll? v) (not (ref? v)))
      ::coll
      (type v))))

(defmethod map-entry :default
  [i k v]
  [:div {:key i}
   [value k]
   [value v {:inline true}]])

(defn- pos
  [e]
  {:x (.-clientX e)
   :y (.-clientY e)})

(defn- on-click
  [ref]
  (fn [e]
    (f/disp [::ui/open-ref-panel ref])))

(defn- on-context-click
  [value]
  (fn [e]
    (.preventDefault e)
    (f/disp [::ui/open-context value (pos e)])))

(defmethod value :default
  [x & _]
  [:div {:class           "reflet-value-default"
         :on-context-menu (on-context-click x)}
   [:div (str x)]])

(defmethod value nil
  [& _]
  [:div {:class "reflet-nil"} "nil"])

(defmethod value js/Number
  [n & _]
  [:div {:class           "reflet-number"
         :on-context-menu (on-context-click n)}
   n])

(defmethod value js/String
  [s & _]
  [:div {:class           "reflet-string"
         :on-context-menu (on-context-click s)}
   [:span (str \" s \")]])

(defn- attr->ns
  [attr]
  (if (keyword? attr)
    (namespace attr)
    attr))

(defn value-ref
  [ref & {:keys [on-click]}]
  (let [[attr value] ref]
    [:div {:class           "reflet-ref"
           :on-click        on-click
           :on-context-menu (on-context-click ref)}
     (attr->ns attr) "@"
     (if (uuid? value)
       (.slice (str value) (- uuid-condensed-chars))
       (str value))]))

(defmethod value ::ref
  [ref & _]
  (value-ref ref :on-click (on-click ref)))

(defmethod value Keyword
  [k & _]
  [:div {:class           "reflet-keyword"
         :on-context-menu (on-context-click k)}
   [:span ":"]
   [:span (apply str (rest (str k \u200E)))]])

(defmethod value js/Function
  [f & _]
  [:div {:class "reflet-function"}
   "(fn ...)"])

(defmethod value js/Boolean
  [x & _]
  [:div {:class "reflet-boolean"}
   (str x)])

(defmethod value cljs.core/UUID
  [uuid & _]
  [:div {:class "reflet-uuid"
         :on-context-menu (on-context-click uuid)}
   (->> (- uuid-condensed-chars)
        (.slice (str uuid))
        (str "#uuid ..."))])

(defn- coll-class
  [coll expand? {:keys [inline class]}]
  [class
   (cond
     (map? coll)    "reflet-map"
     (vector? coll) "reflet-vec"
     (list? coll)   "reflet-list"
     (set? coll)    "reflet-set")
   (when inline
     "reflet-coll-inline")
   (when expand?
     "reflet-coll-expand")])

(defn expander-toggle
  [self]
  (when self
    (fn [e]
      (.preventDefault e)
      (f/disp [::impl/toggle-expand self]))))

(defn wrap-expander
  [opts coll expr]
  (f/with-ref* {:cmp/uuid [coll/self]}
    (let [expand? @(f/sub [::impl/expand? self])]
      [:<>
       [:div {:class (coll-class coll expand? opts)}
        expr
        (when (not-empty coll)
          (g/coll-expander
           {:class    "reflet-coll-expander"
            :on-click (expander-toggle self)}))]
       (when (and (not-empty coll) expand?)
         [value coll])])))

(defmethod value ::map
  [m & [opts]]
  (wrap-expander opts m
    [:div {:class "reflet-map-data"}
     (doall
      (map-indexed
       (fn [i [k v]]
         ^{:key i} [map-entry i k v])
       m))]))

(defn- coll-item
  [x]
  (f/with-ref* {:el/uuid [el]}
    [:div {:ref   (i/el! el)
           :style @(f/sub [::impl/coll-item-style el])}
     [value x]]))

(defn- coll-shrink
  [el]
  (when @(f/sub [::i/grab el])
    "reflet-coll-shrink"))

(defmethod value ::coll
  [coll & [opts]]
  (f/with-ref* {:el/uuid [el]}
    (wrap-expander opts coll
      [:div {:ref   (i/el! el)
             :class ["reflet-coll-data" (coll-shrink el)]}
       (doall
        (map-indexed
         (fn [i x]
           ^{:key i} [coll-item x])
         coll))])))
