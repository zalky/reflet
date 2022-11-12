(ns reflet.debug.ui
  (:require [react-dom :as react-dom]
            [reagent.core :as r]
            [reagent.dom :as dom]
            [reflet.core :as f]
            [reflet.debug :as d]
            [reflet.debug.glyphs :as g]
            [reflet.debug.ui.data :as data]
            [reflet.debug.ui.data.impl :as data-impl]
            [reflet.debug.ui.impl :as impl]
            [reflet.interop :as i]))

(def component-name-re
  #"(.*\.)*([^.]+)")

(defn- component-name
  [id]
  (let [[_ ns n] (re-find component-name-re (namespace id))]
    n))

(defn- props-name
  [{:debug/keys [id ns line]}]
  [:div {:class "reflet-props-title"}
   (if (and id ns line)
     [:<>
      [:span (component-name id)]
      [:span "in"]
      [:span ns " : L" line]]
     [:span "with-ref removed"])])

(defn- dragging-class
  []
  (case @(f/sub [::impl/dragging])
    ::impl/move   "reflet-move"
    ::impl/resize "reflet-resize"
    nil))

(defmulti render
  :debug/type)

(defn- mark-expanded
  [{:debug/keys [tap]}]
  (let [t (f/sub [::impl/tap tap])]
    [:div {:on-click #(f/disp [::impl/open-prop tap])}
     [g/mark-icon]
     [:div (some-> @t props-name)]]))

(defmethod render :debug.type/mark
  [{:debug/keys [tap] :as props}]
  (f/with-ref* {:debug/id [debug/self]
                :el/uuid  [debug/el]
                :in       props}
    (let [rect  (f/sub [::impl/rect self])
          state (f/sub [::impl/node-fsm self])
          cb    #(f/disp [::impl/set-rect self el tap])]
      (f/once (f/disp [::impl/set-props self props]))
      (when @state
        [:div {:ref   (i/el! el :cb cb)
               :class "reflet-node"
               :style @rect}
         [:div {:on-mouse-enter #(f/disp [::impl/open self])
                :on-mouse-leave #(f/disp [::impl/close self])
                :on-click       #(f/disp [::impl/select el])
                :class          ["reflet-mark"
                                 (when (= @state ::impl/open)
                                   "reflet-open")]}
          [:div {:class "reflet-context"}
           [mark-expanded props]]
          [g/mark-icon]]]))))

(defn- marks-expanded
  [{:debug/keys [group]}]
  [:div {:class "reflet-context"}
   (doall
    (for [{id  :debug/self
           :as n} group]
      ^{:key id} [mark-expanded n]))])

(defmethod render :debug.type/mark-group
  [{:debug/keys [centroid] :as props}]
  (f/with-ref* {:debug/id [debug/self]
                :el/uuid  [debug/el]
                :in       props}
    (let [rect  (f/sub [::impl/rect self])
          state (f/sub [::impl/node-fsm self])
          cb    #(f/disp [::impl/set-rect self el centroid])]
      (f/once (f/disp [::impl/set-props self props]))
      (when @state
        [:div {:ref   (i/el! el :cb cb)
               :class "reflet-node"
               :style @rect}
         [:div {:on-mouse-enter #(f/disp [::impl/open self])
                :on-mouse-leave #(f/disp [::impl/close self])
                :on-click       #(f/disp [::impl/select el])
                :class          ["reflet-mark-group"
                                 (when (= @state ::impl/open)
                                   "reflet-open")]}
          [marks-expanded props]
          [g/mark-icon {:group true}]]]))))

(defmulti header
  :debug/type)

(defmethod header :debug.type/props-panel
  [{:debug/keys [self tap]}]
  (let [props    (f/sub [::impl/props-panel self])
        dragging (f/sub [::impl/dragging])
        on-close #(f/disp [::impl/close-props-panel tap])
        on-drag  #(f/disp-sync [::impl/drag! ::impl/move self %])]
    [:div {:class         "reflet-header"
           :on-mouse-down on-drag}
     (props-name @props)
     [g/x {:class         ["reflet-close"]
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
  [{:debug/keys [self el]}]
  (when-let [p @(f/sub [::impl/props-panel self])]
    (f/once [(f/disp [::impl/set-rect self el])
             (f/disp [::impl/set-height self el])])
    [data/value (:debug/props p)]))

(defn- display?
  [state]
  (isa? impl/state-hierarchy state ::impl/display))

(defmethod render :debug.type/props-panel
  [{:debug/keys [tap] :as props}]
  (f/with-ref* {:debug/id [debug/self]
                :el/uuid  [debug/el]
                :in       props}
    (let [rect  (f/sub [::impl/rect-quantized self el])
          state (f/sub [::impl/panel-fsm self el tap])]
      (f/once (f/disp [::impl/set-props self props]))
      (when (display? @state)
        [:div {:ref      (i/el! el)
               :style    @rect
               :on-click #(f/disp [::impl/select el])
               :class    ["reflet-panel" (dragging-class)]}
         [:div {:class "reflet-content"}
          [header props]
          [props-content props]]
         [handle props]
         (drop-shadow)]))))

(defmethod header :debug.type/ref-panel
  [{:debug/keys [self ref]}]
  (let [lens     (f/sub [::impl/lens self])
        on-back  #(f/disp [::impl/clear-lens self])
        on-close #(f/disp [::impl/close-ref-panel self])
        on-drag  #(f/disp-sync [::impl/drag! ::impl/move self %])]
    [:div {:class         "reflet-header"
           :on-mouse-down on-drag}
     (when @lens
       [g/back {:class         "reflet-control"
                :on-mouse-down (f/stop-prop on-back)}])
     [:div {:class "reflet-ref-title"}
      [data/value-ref ref]]
     [g/x {:class         "reflet-close"
           :on-mouse-down (f/stop-prop on-close)}]]))

(defmulti ref-content
  (comp first :debug/ref))

(defmulti ref-lens
  (fn [{:debug/keys [self]}]
    @(f/sub [::impl/lens self])))

(defmethod ref-lens :debug.lens/db
  [{:debug/keys [self ref el]}]
  (f/once (f/disp [::impl/set-height self el]))
  (if-let [e @(f/sub [::data-impl/entity ref])]
    [data/value e]
    [:div {:class "reflet-no-data"}
     [:span "No Data"]]))

(defn- event-trace
  [{t :t
    e :event}]
  [:div
   [:div {:class "reflet-divider"}]
   [:div t]
   [data/value e]])

(defmethod ref-lens :debug.lens/events
  [{:debug/keys [self ref el]}]
  (f/once (f/disp [::impl/set-height self el]))
  (if-let [events @(f/sub [::d/e->events ref])]
    [:div {:class "reflet-event-lens"}
     (doall
      (map-indexed
       (fn [i e]
         ^{:key i} [event-trace e])
       events))]
    [:div {:class "reflet-no-data"}
     [:span "No Events"]]))

(defn- fsm-transition
  [{:keys [clause prev-state]}]
  [:div {:class "reflet-transition"}
   [data/value prev-state]
   [:div "\u0394"]
   [data/value (:to clause)]])

(defn- fsm-trace
  [[[t event] transitions]]
  [:div
   [:div {:class "reflet-divider"}]
   [:div t]
   [data/value event]
   (doall
    (map-indexed
     (fn [i t]
       [:<> {:key i}
        [g/fsm {:class "reflet-fsm-glyph"}]
        [data/value (:fsm-v t)]
        [fsm-transition t]])
     transitions))])

(defmethod ref-lens :debug.lens/fsm
  [{:debug/keys [self ref el]}]
  (f/once (f/disp [::impl/set-height self el]))
  (if-let [transitions @(f/sub [::d/fsm->transitions ref])]
    [:div {:class "reflet-fsm-lens"}
     (doall
      (map-indexed
       (fn [i t]
         ^{:key i} [fsm-trace t])
       transitions))]
    [:div {:class "reflet-no-data"}
     [:span "No Events"]]))

(defmethod ref-lens :default
  [{:debug/keys [self el]}]
  (let [cb-d #(f/disp [::impl/set-lens self :debug.lens/db])
        cb-e #(f/disp [::impl/set-lens self :debug.lens/events])
        cb-p #(f/disp [::impl/set-lens self :debug.lens/pull])
        cb-f #(f/disp [::impl/set-lens self :debug.lens/fsm])]
    (f/once [(f/disp [::impl/set-rect self el])
             (f/disp [::impl/set-height self el])])
    [:div {:class "reflet-set-lens"}
     [:div {:on-click cb-d} "db"]
     [:div {:on-click cb-e} "events"]
     [:div {:on-click cb-p} "query"]
     [:div {:on-click cb-f} "FSM"]]))

(defmethod ref-content :default
  [props]
  [ref-lens props])

(defmethod ref-content :el/uuid
  [{:debug/keys [self el ref]}]
  (f/once [(f/disp [::impl/set-rect self el])
           (f/disp [::impl/set-height self el])])
  (if-let [^js el @(f/sub [::i/grab ref])]
    [:div {:class "reflet-html"}
     (.-outerHTML el)]
    [:div {:class "reflet-no-data"}
     [:span "No Element"]]))

(defmethod render :debug.type/ref-panel
  [{:debug/keys [ref] :as props}]
  (f/with-ref* {:debug/id [debug/self]
                :el/uuid  [debug/el]
                :in       props}
    (let [rect  (f/sub [::impl/rect-quantized self el])
          state (f/sub [::impl/panel-fsm self el ref])]
      (f/once (f/disp [::impl/set-props self props]))
      (when (display? @state)
        [:div {:ref      (i/el! el)
               :style    @rect
               :on-click #(f/disp [::impl/select el])
               :class    ["reflet-panel" (dragging-class)]}
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
           :as node} @(f/sub [::impl/overlay])]
      ^{:key id} [render node]))])

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
  "On mount, the tap must be situated in the target dom to access dom
  relationships like parent, siblings, and position. After that
  initial tap, the tap element becomes transparent as far as the dom
  structure is concerned, situated elsewhere using a portal. The
  reactive tap then updates the debugger with changes in with-ref
  props. ::d/tap must happen after the ::d/untap of the previous react
  lifecycle. To guarantee this, ::d/tap must not be dispatched in the
  render phase, for example in the bindings of a `with-let`, where
  the ::d/tap handler will run immediately after the first render, but
  before the previous lifecycle's cleanup. It is safe to use in either
  the `:ref` callback, the `:component-did-mount`, or
  `:component-did-update` phase of the component lifecycle."
  [props target]
  (if-not @target
    [:div {:class "reflet-tap"
           :ref   (partial init-tap props target)}]
    (-> [reactive-tap props]
        (r/as-element)
        (react-dom/createPortal (body-el)))))

(defn- upsert-css!
  []
  (when-not (js/document.getElementById "reflet-css")
    (let [el (js/document.createElement "style")]
      (set! (.-id el) "reflet-css")
      (set! (.-innerHTML el) "")        ; Nothing for now
      (js/document.body.appendChild el))))

(defn- upsert-overlay-el!
  []
  (or (overlay-el)
      (let [el (.createElement js/document "div")]
        (set! (.-id el) "reflet-overlay")
        (.add (.-classList el) "reflet-overlay")
        (.appendChild (body-el) el)
        el)))

(defn load-debugger!
  []
  (set! d/tap-fn tap)
  (upsert-css!)
  (->> (upsert-overlay-el!)
       (dom/render [overlay])))
