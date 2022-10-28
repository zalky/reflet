(ns reflet.debug.ui
  (:require [react-dom :as react-dom]
            [reagent.core :as r]
            [reagent.dom :as dom]
            [reflet.core :as f]
            [reflet.debug :as d]
            [reflet.debug.glyphs :as g]
            [reflet.debug.ui.data :as data]
            [reflet.debug.ui.data.impl :as datai]
            [reflet.debug.ui.impl :as impl]
            [reflet.interop :as i]))

(def component-name-re
  #"(.*)\.([^.]+)+")

(defn- props-name
  [{:debug/keys [fn line]}]
  (if (and fn line)
    (let [ns (namespace fn)
          n  (name fn)]
      [:span (str ns "/" n " : L" line)])
    [:span "with-ref removed"]))

(defmulti render
  :debug/type)

(defn- mark-expanded
  [{:debug/keys [tap]}]
  (let [t (f/sub [::impl/tap tap])]
    [:div {:on-click #(f/disp [::impl/open-prop tap])}
     [g/mark-icon]
     [:div (some-> @t props-name)]]))

(defmethod render :debug.type/mark
  [{:debug/keys [type tap] :as props}]
  (f/with-ref* {:cmp/uuid [debug/self]
                :el/uuid  [debug/el]}
    (let [state (f/sub [::impl/node-fsm self])
          rect  (f/sub [::impl/rect self])
          cb    #(do (f/disp [::impl/set-rect self type el tap])
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
    (for [{id  :debug/self
           :as n} group]
      ^{:key id} [mark-expanded n]))])

(defmethod render :debug.type/group
  [{:debug/keys [type centroid] :as props}]
  (f/with-ref* {:cmp/uuid [debug/self]
                :el/uuid  [debug/el]}
    (let [state (f/sub [::impl/node-fsm self])
          rect  (f/sub [::impl/rect self])
          cb    #(do (f/disp [::impl/set-rect self type el centroid])
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

(defmulti header
  :debug/type)

(defmethod header :debug.type/props-cmp
  [{:debug/keys [self tap]}]
  (let [props    (f/sub [::impl/props-cmp self])
        dragging (f/sub [::impl/dragging])
        on-close #(f/disp [::impl/close-prop tap])
        on-drag  #(f/disp-sync [::impl/drag! ::impl/move self %])]
    [:div {:class         ["reflet-header" (when @dragging "reflet-dragging")]
           :on-mouse-down on-drag}
     (props-name @props)
     [g/x {:class         "reflet-control"
           :on-mouse-down (f/stop-prop on-close)}]]))

(defn- handle
  [{:debug/keys [self]}]
  (let [on-drag #(f/disp-sync [::impl/drag! ::impl/resize self %])]
    [g/handle {:class         "reflet-panel-handle"
               :on-mouse-down on-drag}]))

(defn drop-shadow
  []
  [:div {:class "reflet-panel-shadow"}])

(defn- props-content
  [{:debug/keys [self]}]
  (when-let [p @(f/sub [::impl/props-cmp self])]
    (f/once (f/disp [::impl/ready-to-size self]))
    [data/debug-value (:debug/props p)]))

(defmethod render :debug.type/props-cmp
  [{:debug/keys [type] :as props}]
  (f/with-ref* {:cmp/uuid [debug/self]
                :el/uuid  [debug/el]
                :in       props}
    (let [rect  (f/sub [::impl/rect self])
          state (f/sub [::impl/panel-fsm self type el])]
      (f/once (f/disp [::impl/set-props self props]))
      (when (isa? impl/state-hierarchy @state ::impl/display)
        [:div {:ref   (i/el! el)
               :class "reflet-panel"
               :style @rect}
         [:div {:class "reflet-content"}
          [header props]
          [props-content props]]
         [handle props]
         (drop-shadow)]))))

(defmethod header :debug.type/ref
  [{:debug/keys [self ref]}]
  (let [dragging (f/sub [::impl/dragging])
        on-close #(f/disp [::impl/close-ref ref])
        on-drag  #(f/disp-sync [::impl/drag! ::impl/move self %])]
    [:div {:class         ["reflet-header" (when @dragging "reflet-dragging")]
           :on-mouse-down on-drag}
     [data/debug-value ref]
     [g/x {:class         "reflet-control"
           :on-mouse-down (f/stop-prop on-close)}]]))

(defn- ref-content
  [{:debug/keys [self ref]}]
  (let [e (f/sub [::datai/entity ref])]
    (f/once (f/disp [::impl/ready-to-size self]))
    [data/debug-value @e]))

(defmethod render :debug.type/ref
  [{:debug/keys [type] :as props}]
  (f/with-ref* {:cmp/uuid [debug/self]
                :el/uuid  [debug/el]
                :in       props}
    (let [rect  (f/sub [::impl/rect self])
          state (f/sub [::impl/panel-fsm self type el])]
      (f/once (f/disp [::impl/set-props self props]))
      (when (isa? impl/state-hierarchy @state ::impl/display)
        [:div {:ref   (i/el! el)
               :class "reflet-panel"
               :style @rect}
         [:div {:class "reflet-content"}
          [header props]
          [ref-content props]]
         [handle props]
         (drop-shadow)]))))

(defn- overlay
  []
  [:<>
   (doall
    (for [{id  :debug/self
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

(defn- init-tap
  [props target tap-el]
  (let [ref (find props :debug/id)]
    (some->> tap-el
             (find-tap-point)
             (reset! target)
             (impl/rect)
             (assoc props :debug/rect)
             (vector ::d/tap ref)
             (f/disp))))

(defn reactive-tap
  [_]
  (r/create-class
   {:component-did-update
    (f/props-did-update-handler
     (fn [_ props]
       (let [ref (find props :debug/id)]
         (f/disp [::d/tap ref props]))))

    :reagent-render
    (fn [_]
      [:div {:class "reflet-reactive-tap"}])}))

(defn tap
  "::d/tap must happen after the ::d/untap of the previous react
  lifecycle. To guarantee this, ::d/tap must be invoked in either the
  `:ref` callback, or the `:component-did-mount` phase of the
  component lifecycle. Must not dispatch ::d/tap in a `with-let`,
  where it will happen during the first render."
  [props target]
  (if-not @target
    [:div {:class "reflet-tap"
           :ref   (partial init-tap props target)}]
    (-> [reactive-tap props]
        (r/as-element)
        (react-dom/createPortal (body-el)))))

(defn- upsert-overlay-el!
  []
  (or (overlay-el)
      (let [el (.createElement js/document "div")]
        (.setAttribute el "id" "reflet-overlay")
        (.appendChild (body-el) el)
        el)))

(defn load-debugger!
  []
  (set! d/*debug* tap)
  (->> (upsert-overlay-el!)
       (dom/render [overlay])))
