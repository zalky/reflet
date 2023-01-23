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
  (fn [x]
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
   [value v]])

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
  [x]
  [:div {:class           "reflet-value-default"
         :on-context-menu (on-context-click x)}
   [:div (str x)]])

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
  [ref]
  (value-ref ref :on-click (on-click ref)))

(defmethod value Keyword
  [k]
  [:div {:class           "reflet-keyword"
         :on-context-menu (on-context-click k)}
   [:span ":"]
   [:span (apply str (rest (str k \u200E)))]])

(defmethod value js/Function
  [f]
  [:div {:class "reflet-function"}
   "(fn ...)"])

(defmethod value js/Boolean
  [x]
  [:div {:class "reflet-boolean"}
   (str x)])

(defmethod value cljs.core/UUID
  [uuid]
  [:div {:class "reflet-uuid"
         :on-context-menu (on-context-click uuid)}
   (->> (- uuid-condensed-chars)
        (.slice (str uuid))
        (str "#uuid ..."))])

(defmethod value ::map
  [m]
  [:div {:class "reflet-map"}
   [:div {:class "reflet-map-data"}
    (doall
     (map-indexed
      (fn [i [k v]]
        ^{:key i} [map-entry i k v])
      m))]])

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
  [coll]
  (f/with-ref* {:el/uuid [el]}
    [:div {:class (cond
                    (vector? coll) "reflet-vec"
                    (list? coll)   "reflet-list"
                    (set? coll)    "reflet-set"
                    :else          nil)}
     [:div {:ref   (i/el! el)
            :class ["reflet-coll-data" (coll-shrink el)]}
      (doall
       (map-indexed
        (fn [i x]
          ^{:key i} [coll-item x])
        coll))]]))

(declare expander)

(defn- inline-coll
  [coll]
  [:div (doall
         (map-indexed
          (fn [i x]
            (with-meta
              (if (and (coll? x) (not (ref? x)))
                [expander nil x]
                [value x])
              {:key i}))
          coll))])

(defn expander-toggle
  [self]
  (when self
    (fn [e]
      (.preventDefault e)
      (f/disp [::impl/toggle-expand self]))))

(defn- expander
  [{:expander/keys [self]} v]
  (let [e (when self (g/coll-expander))]
    [:div {:on-click (expander-toggle self)
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
