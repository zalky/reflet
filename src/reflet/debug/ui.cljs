(ns reflet.debug.ui
  (:require [react-dom :as react-dom]
            [reagent.core :as r]
            [reagent.dom :as dom]
            [reflet.core :as f]
            [reflet.debug :as d]
            [reflet.debug.glyphs :as g]
            [reflet.debug.ui.impl :as impl]
            [reflet.interop :as i]))

(defn ref?
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
  [[attr uuid]]
  [:div {:class "debug-ref"}
   "@" (subs (str uuid) 0 6)])

(defmethod debug-value ::string
  [s]
  [:div {:class "debug-string"}
   (str \" s \")])

(defmethod debug-value ::map
  [m]
  [:div {:class "debug-map"}
   [:div {:class "debug-map-data"}
    (map-indexed
     (fn [i [k ref]]
       (when (not= k ::f/debug-id)
         [:div {:key i}
          [debug-value k]
          [debug-value ref]]))
     m)]])

(defmethod debug-value Keyword
  [k]
  [:div {:class "debug-keyword"}
   (if-let [ns (namespace k)]
     [:<>
      [:span (str ":" ns "/")]
      [:span (name k)]]
     [:span (str k)])])

(defmethod debug-value :default
  [x]
  [:div (str x)])

(defmulti render
  :debug/type)

(defmethod render :debug.type/mark
  [{:debug/keys [tap] :as props}]
  (f/with-ref {:cmp/uuid [debug/self]
               :el/uuid  [debug/el]
               :debug    false}
    (let [rect (f/subscribe [::impl/rect self])
          cb   #(f/dispatch [::impl/init-node self props %])]
      [:div {:ref      (i/el! el :cb cb)
             :class    "debug-mark"
             :style    @rect
             :on-click #(f/dispatch [::impl/toggle tap])}
       [g/mark-icon]])))

(defmethod render :debug.type/group
  [{:debug/keys [group pos]}]
  (f/with-ref {:cmp/uuid [debug/self]
               :debug    false}
    [:div {:class    "debug-mark group"
           :style    {:left (:x pos)
                      :top  (:y pos)}
           :on-click #(f/dispatch [::impl/toggle self])}
     [g/mark-icon {:group true}]]))

(defn- debug-refs
  [{:debug/keys [self]}]
  (when-let [props @(f/subscribe [::impl/props self])]
    [:div {:class "debug-refs"}
     [debug-value (:debug/refs props)]]))

(def component-name-re
  #"(.*)\.([^.]+)+")

(defn- component-name
  [s]
  (if-let [[_ ns n] (re-find component-name-re s)]
    (str ns "/" n)
    s))

(defn- debug-header
  [{:debug/keys [self]}]
  (let [props    (f/subscribe [::impl/props self])
        dragging (f/subscribe [::impl/dragging])
        on-md    #(f/dispatch-sync [::impl/drag! % self])]
    [:div {:class ["debug-header" (when @dragging "dragging")]
           :on-mouse-down on-md}
     (some-> @props
             (:debug/name)
             (component-name))]))

(defmethod render :debug.type/props
  [{:debug/keys [tap] :as props}]
  (f/with-ref {:cmp/uuid [debug/self]
               :in       props
               :debug    false}
    (f/with-ref {:el/uuid [debug/el]}
      (let [state (f/subscribe [::impl/panel self tap])
            rect  (f/subscribe [::impl/rect self])
            cb    #(f/dispatch [::impl/init-node self props %])]
        (when @state
          [:div {:ref   (i/el! el :cb cb)
                 :class "debug-panel"
                 :style @rect}
           [:div {:class "debug-content"}
            [debug-header props]
            [debug-refs props]]
           [:div]])))))

(defn overlay-id
  [{tap :debug/tap
    t   :debug/type}]
  (str t (second tap)))

(defn overlay
  []
  (let [nodes (f/subscribe [::impl/overlay])]
    [:div
     (doall
      (for [n @nodes]
        ^{:key (overlay-id n)} [render n]))]))

(defn- body-el
  []
  (.querySelector js/document "body"))

(defn- overlay-el
  []
  (.querySelector js/document "#reflet-debug-overlay"))

(defn- find-tap-point
  "Search siblings for first element that is not a debug tap. If no
  siblings, choose parent. An example of where there could be no
  sibling is if a component returns an empty fragment."
  [tap-el]
  (loop [el (.-nextSibling tap-el)]
    (if el
      (let [c (.-className el)]
        (if (= c "debug-tap")
          (recur (.-nextSibling el))
          el))
      (.-parentElement tap-el))))

(defn- tap
  [props target tap-el]
  (some->> tap-el
           (find-tap-point)
           (reset! target)
           (impl/rect)
           (assoc props :debug/rect)
           (vector ::d/tap)
           (f/dispatch)))

(defn debug-tap
  [target props]
  [:div {:class "debug-tap"
         :ref   (partial tap props target)}])

(defn upsert-overlay-el!
  []
  (or (overlay-el)
      (let [el (.createElement js/document "div")]
        (.setAttribute el "id" "reflet-debug-overlay")
        (.appendChild (body-el) el)
        el)))

(defn load-debugger!
  []
  (set! d/*debug* debug-tap)
  (->> (upsert-overlay-el!)
       (dom/render [overlay])))
