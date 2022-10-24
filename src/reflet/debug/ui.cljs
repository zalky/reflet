(ns reflet.debug.ui
  (:require [react-dom :as react-dom]
            [reagent.core :as r]
            [reagent.dom :as dom]
            [reflet.core :as f]
            [reflet.debug :as d]
            [reflet.debug.glyphs :as g]
            [reflet.debug.ui.impl :as impl]
            [reflet.interop :as i]))

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
  [[attr uuid]]
  [:div {:class "reflet-ref"}
   "@" (subs (str uuid) 0 6)])

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

(defmulti render
  :debug/type)

(defmethod render :debug.type/mark
  [{:debug/keys [tap] :as props}]
  (f/with-ref* {:cmp/uuid [debug/self]
                :el/uuid  [debug/el]}
    (let [rect (f/sub [::impl/rect self])
          cb   #(do (f/disp [::impl/set-rect self tap el])
                    (f/disp [::impl/set-props self props]))]
      [:div {:ref      (i/el! el :cb cb)
             :class    "reflet-mark"
             :style    @rect
             :on-click #(f/disp [::impl/toggle tap])}
       [g/mark-icon]])))

(defmethod render :debug.type/group
  [{:debug/keys [centroid] :as props}]
  (f/with-ref* {:cmp/uuid [debug/self]
                :el/uuid  [debug/el]}
    (let [rect (f/sub [::impl/rect self])
          cb   #(do (f/disp [::impl/set-centroid self centroid el])
                    (f/disp [::impl/set-props self props]))]
      [:div {:ref      (i/el! el :cb cb)
             :class    "reflet-group"
             :style    @rect
             :on-click #(f/disp [::impl/toggle self])}
       [g/mark-icon {:group true}]])))

(defn- debug-refs
  [{:debug/keys [self]}]
  (when-let [p @(f/sub [::impl/props self])]
    (f/once (f/disp [::impl/props-ready self]))
    [:div {:class "reflet-refs"}
     [debug-value (:debug/refs p)]]))

(def component-name-re
  #"(.*)\.([^.]+)+")

(defn- props-name
  [{:debug/keys [id line]}]
  (if-let [[_ ns n] (re-find component-name-re (namespace id))]
    [:span (str ns "/" n  " : L" line)]))

(defn- debug-header
  [{:debug/keys [self tap]}]
  (let [props    (f/sub [::impl/props self])
        dragging (f/sub [::impl/dragging])
        on-drag  #(f/disp-sync [::impl/drag! ::impl/move self %])
        on-close #(f/disp [::impl/toggle tap])]
    [:div {:class         ["reflet-header" (when @dragging "reflet-dragging")]
           :on-mouse-down on-drag}
     (some-> @props props-name)
     [g/x {:class         "reflet-control"
           :on-mouse-down (f/stop-prop on-close)}]]))

(defn- handle
  [{:debug/keys [self]}]
  (let [on-drag #(f/disp-sync [::impl/drag! ::impl/resize self %])]
    [g/handle {:class         "reflet-panel-handle"
               :on-mouse-down on-drag}]))

(defmethod render :debug.type/props
  [{:debug/keys [tap] :as props}]
  (f/with-ref* {:cmp/uuid [debug/self]
                :el/uuid  [debug/el]
                :in       props}
    (let [state (f/sub [::impl/panel self tap el])
          rect  (f/sub [::impl/rect self])]
      (f/once (f/disp [::impl/set-props self props]))
      (when (isa? impl/state-h @state ::impl/display)
        [:div {:ref   (i/el! el)
               :class "reflet-panel"
               :style @rect}
         [:div {:class "reflet-content"}
          [debug-header props]
          [debug-refs props]]
         [:div {:class "reflet-panel-shadow"}]
         [handle props]]))))

(defn- overlay
  []
  [:div
   (doall
    (for [{id  :overlay/id
           :as n} @(f/sub [::impl/overlay])]
      ^{:key id} [render n]))])

(defn- body-el
  []
  (.querySelector js/document "body"))

(defn- overlay-el
  []
  (.querySelector js/document "#reflet-overlay"))

(defn- find-tap-point
  "Search siblings for first element that is not a debug tap. If no
  siblings, choose parent. An example of where there could be no
  sibling is if a component returns an empty fragment."
  [tap-el]
  (loop [el (.-nextSibling tap-el)]
    (if el
      (let [c (.-className el)]
        (if (= c "reflet-tap")
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
           (f/disp)))

(defn debug-tap
  "::d/tap must happen after the ::d/untap of the previous react
  lifecycle. To guarantee this, ::d/tap must be invoked in either the
  `:ref` callback, or the `:component-did-mount` phase of the
  component lifecycle. Must not dispatch ::d/tap in a `with-let`,
  where it will happen during the first render."
  [target props]
  (if-not @target
    [:div {:class "reflet-tap"
           :ref   (partial tap props target)}]
    [:<>]))

(defn- upsert-overlay-el!
  []
  (or (overlay-el)
      (let [el (.createElement js/document "div")]
        (.setAttribute el "id" "reflet-overlay")
        (.appendChild (body-el) el)
        el)))

(defn load-debugger!
  []
  (set! d/*debug* debug-tap)
  (->> (upsert-overlay-el!)
       (dom/render [overlay])))
