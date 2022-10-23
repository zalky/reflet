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
  (f/with-ref* {:cmp/uuid [debug/self]
                :el/uuid  [debug/el]}
    (let [rect (f/subscribe [::impl/rect self])
          cb   #(do (f/dispatch [::impl/set-rect self tap el])
                    (f/dispatch [::impl/init-props self props]))]
      [:div {:ref      (i/el! el :cb cb)
             :class    "debug-mark"
             :style    @rect
             :on-click #(f/dispatch [::impl/toggle tap])}
       [g/mark-icon]])))

(defmethod render :debug.type/group
  [{:debug/keys [group pos]}]
  (f/with-ref* {:cmp/uuid [debug/self]}
    [:div {:class    "debug-mark group"
           :style    {:left (:x pos)
                      :top  (:y pos)}
           :on-click #(f/dispatch [::impl/toggle self])}
     [g/mark-icon {:group true}]]))

(defn- debug-refs
  [{:debug/keys [self]}]
  (when-let [p @(f/subscribe [::impl/props self])]
    (f/once (f/dispatch [::impl/props-ready self]))
    [:div {:class "debug-refs"}
     [debug-value (:debug/refs p)]]))

(def component-name-re
  #"(.*)\.([^.]+)+")

(defn- props-name
  [{:debug/keys [id line]}]
  (if-let [[_ ns n] (re-find component-name-re (namespace id))]
    [:span (str ns "/" n  " : L" line)]))

(defn- debug-header
  [{:debug/keys [self]}]
  (let [props    (f/subscribe [::impl/props self])
        dragging (f/subscribe [::impl/dragging])
        on-md    #(f/dispatch-sync [::impl/drag! self %])]
    [:div {:class ["debug-header" (when @dragging "dragging")]
           :on-mouse-down on-md}
     (some-> @props props-name)]))

(defmethod render :debug.type/props
  [{:debug/keys [tap] :as props}]
  (f/with-ref* {:cmp/uuid [debug/self]
                :el/uuid  [debug/el]
                :in       props}
    (let [state (f/subscribe [::impl/panel self tap el])
          rect  (f/subscribe [::impl/rect self])]
      (f/once (f/dispatch [::impl/init-props self props]))
      (when (isa? impl/state-h @state ::impl/display)
        [:div {:ref   (i/el! el)
               :class "debug-panel"
               :style @rect}
         [:div {:class "debug-content"}
          [debug-header props]
          [debug-refs props]]
         [:div]]))))

(defn overlay-id
  [{tap :debug/tap
    t   :debug/type}]
  (str t (second tap)))

(defn overlay
  []
  [:div
   (doall
    (for [n @(f/subscribe [::impl/overlay])]
      ^{:key (:overlay/id n)} [render n]))])

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
           (vector ::d/tap (find props :debug/id))
           (f/dispatch)))

(defn debug-tap
  "::d/tap must happen after the ::d/untap of the previous react
  lifecycle. To guarantee this, ::d/tap must be invoked in either the
  `:ref` callback, or the `:component-did-mount` phase of the
  component lifecycle. Must not dispatch ::d/tap in a `with-let`,
  where it will happen during the first render."
  [target props]
  (if-not @target
    [:div {:class "debug-tap"
           :ref   (partial tap props target)}]
    [:<>]))

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
