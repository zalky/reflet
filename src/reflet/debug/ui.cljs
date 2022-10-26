(ns reflet.debug.ui
  (:require [react-dom :as react-dom]
            [reagent.core :as r]
            [reagent.dom :as dom]
            [reflet.core :as f]
            [reflet.debug :as d]
            [reflet.debug.glyphs :as g]
            [reflet.debug.ui.data :as data]
            [reflet.debug.ui.impl :as impl]
            [reflet.interop :as i]))

(def component-name-re
  #"(.*)\.([^.]+)+")

(defn- props-name
  [{:debug/keys [id line]}]
  (if-let [[_ ns n] (re-find component-name-re (namespace id))]
    [:span (str ns "/" n  " : L" line)]))

(defmulti render
  :debug/type)

(defn- mark-expanded
  [{:debug/keys [tap]}]
  (let [t (f/sub [::impl/tap tap])]
    [:div {:on-click #(f/disp [::impl/toggle tap])}
     [g/mark-icon]
     [:div (some-> @t props-name)]]))

(defmethod render :debug.type/mark
  [{:debug/keys [tap] :as props}]
  (f/with-ref* {:cmp/uuid [debug/self]
                :el/uuid  [debug/el]}
    (let [state (f/sub [::impl/node-fsm self])
          rect  (f/sub [::impl/rect self])
          cb    #(do (f/disp [::impl/set-tap-rect self el tap])
                     (f/disp [::impl/set-props self props]))
          open? (= @state ::impl/open)]
      [:div {:ref   (i/el! el :cb cb)
             :class "reflet-node"
             :style @rect}
       [:div {:on-mouse-enter #(f/disp [::impl/open self])
              :on-mouse-leave #(f/disp [::impl/close self])
              :class          ["reflet-mark" (when open? "reflet-open")]}
        [:div {:class "reflet-mark-expanded"}
         [mark-expanded props]]
        [g/mark-icon]]])))

(defn- marks-expanded
  [{:debug/keys [group]}]
  [:div {:class "reflet-mark-expanded"}
   (doall
    (for [{id  :overlay/id
           :as n} group]
      ^{:key id} [mark-expanded n]))])

(defmethod render :debug.type/group
  [{:debug/keys [centroid] :as props}]
  (f/with-ref* {:cmp/uuid [debug/self]
                :el/uuid  [debug/el]}
    (let [state (f/sub [::impl/node-fsm self])
          rect  (f/sub [::impl/rect self])
          cb    #(do (f/disp [::impl/set-centroid self el centroid])
                     (f/disp [::impl/set-props self props]))
          open? (= @state ::impl/open)]
      [:div {:ref   (i/el! el :cb cb)
             :class "reflet-node"
             :style @rect}
       [:div {:on-mouse-enter #(f/disp [::impl/open self])
              :on-mouse-leave #(f/disp [::impl/close self])
              :class          ["reflet-group" (when open? "reflet-open")]}
        [marks-expanded props]
        [g/mark-icon {:group true}]]])))

(defn- debug-props-cmp
  [{:debug/keys [self]}]
  (when-let [p @(f/sub [::impl/props-cmp self])]
    (f/once (f/disp [::impl/props-cmp-ready self]))
    [:div {:class "reflet-refs"}
     [data/debug-value (:debug/refs p)]]))

(defmulti debug-header
  :debug/type)

(defmethod debug-header :debug.type/props-cmp
  [{:debug/keys [self tap]}]
  (let [props    (f/sub [::impl/props-cmp self])
        dragging (f/sub [::impl/dragging])
        on-close #(f/disp [::impl/toggle tap])
        on-drag  #(f/disp-sync [::impl/drag! ::impl/move self %])]
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

(defn drop-shadow
  []
  [:<>
   [:div {:class "reflet-panel-shadow"}]
   [:div {:class "reflet-panel-shadow"}]
   [:div {:class "reflet-panel-shadow"}]])

(defmethod render :debug.type/props-cmp
  [{:debug/keys [tap] :as props}]
  (f/with-ref* {:cmp/uuid [debug/self]
                :el/uuid  [debug/el]
                :in       props}
    (let [state (f/sub [::impl/props-cmp-fsm self tap el])
          rect  (f/sub [::impl/rect self])]
      (f/once (f/disp [::impl/set-props self props]))
      (when (isa? impl/state-h @state ::impl/display)
        [:div {:ref   (i/el! el)
               :class "reflet-panel"
               :style @rect}
         [:div {:class "reflet-content"}
          [debug-header props]
          [debug-props-cmp props]]
         [handle props]
         (drop-shadow)]))))

(defmethod debug-header :debug.type/ref
  [{:debug/keys [self ref]}]
  (let [dragging (f/sub [::impl/dragging])
        on-close #(f/disp [::impl/close-ref ref])
        on-drag  #(f/disp-sync [::impl/drag! ::impl/move self %])]
    [:div {:class         ["reflet-header" (when @dragging "reflet-dragging")]
           :on-mouse-down on-drag}
     [data/debug-value ref]
     [g/x {:class         "reflet-control"
           :on-mouse-down (f/stop-prop on-close)}]]))

(defmethod render :debug.type/ref
  [{:debug/keys [ref] :as props}]
  (f/with-ref* {:cmp/uuid [debug/self]
                :el/uuid  [debug/el]
                :in       props}
    (let [rect (f/sub [::impl/rect self])
          cb   #(do (f/disp [::impl/set-rect self el])
                    (f/disp [::impl/set-props self props]))]
      [:div {:ref   (i/el! el :cb cb)
             :class "reflet-panel"
             :style @rect}
       [:div {:class "reflet-content"}
        [debug-header props]]
       [handle props]
       (drop-shadow)])))

(defn- overlay
  []
  [:<>
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
