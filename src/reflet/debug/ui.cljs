(ns reflet.debug.ui
  (:require [react-dom :as react-dom]
            [reagent.core :as r]
            [reagent.dom :as dom]
            [reflet.core :as f]
            [reflet.debug.cluster :as c]
            [reflet.debug.glyphs :as g]
            [reflet.debug.ui.impl :as impl]
            [reflet.interop :as i]
            [reflet.debug :as d]))

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
   [g/brace]
   [:div {:class "debug-map-data"}
    (map-indexed
     (fn [i [k ref]]
       [:div {:key i}
        [debug-value k]
        [debug-value ref]])
     m)]
   [g/brace]])

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

(defn- debug-refs
  [{:debug/keys [refs]}]
  [:div {:class "debug-refs"}
   [debug-value refs]])

(def component-name-re
  #"(.*)\.([^.]+)+")

(defn- component-name
  [s]
  (if-let [[_ ns n] (re-find component-name-re s)]
    (str ns "/" n)
    s))

(defn- debug-header
  [{:debug/keys [name id]}]
  (let [dragging (f/subscribe [::impl/dragging])
        on-md    #(f/dispatch-sync [::impl/drag! % id])]
    [:div {:on-mouse-down on-md
           :class         ["debug-header"
                           (when @dragging "dragging")]}
     (component-name name)]))

(defn- debug-panel
  [{:debug/keys [id] :as props}]
  (f/with-ref {:el/uuid [debug/el]
               :debug   false}
    (let [state (f/subscribe [::impl/panel id])
          geo   (f/subscribe [::impl/panel-geo id])
          cb    #(f/dispatch [::impl/init-panel id %])]
      (when @state
        [:div {:ref   (i/el! el :cb cb)
               :class "debug-panel"
               :style @geo}
         [:div {:class "debug-content"}
          [debug-header props]
          [debug-refs props]]
         [:div]]))))

(defmulti debug-mark
  :debug/type)

(defmethod debug-mark :debug.type/mark
  [{:debug/keys [id]}]
  (f/with-ref {:el/uuid [debug/el]
               :debug   false}
    (let [geo (f/subscribe [::impl/mark-geo id])
          cb  #(f/dispatch [::impl/init-mark id %])]
      [:div {:ref      (i/el! el :cb cb)
             :class    "debug-mark"
             :style    @geo
             :on-click #(f/dispatch [::impl/toggle id])}
       [g/mark-icon]])))

(defmethod debug-mark :debug.type/group
  [{:debug/keys [group centroid]}]
  (f/with-ref {:cmp/uuid [debug/self]
               :debug    false}
    [:div {:class    "debug-mark group"
           :style    {:left (:x centroid)
                      :top  (:y centroid)}
           :on-click #(f/dispatch [::impl/toggle self])}
     [g/mark-icon {:group true}]]))

(defn debug-marks
  []
  [:div
   (for [{:keys [debug/uuid]
          :as   props} @(f/subscribe [::impl/taps-grouped])]
     ^{:key uuid} [debug-mark props])])

(defn- body-el
  []
  (.querySelector js/document "body"))

(defn- loader-el
  []
  (.querySelector js/document "#reflet-debug-loader"))

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
           (vector ::impl/tap)
           (f/dispatch)))

(defn debug-tap
  [target props]
  (if-not @target
    [:div {:class "debug-tap"
           :ref   (partial tap props target)}]
    (-> [debug-panel props]
        (r/as-element)
        (react-dom/createPortal (body-el)))))

(defn upsert-loader-el!
  []
  (or (loader-el)
      (let [el (.createElement js/document "div")]
        (.setAttribute el "id" "reflet-debug-loader")
        (.appendChild (body-el) el)
        el)))

(defn load-debugger!
  []
  (f/dispatch-sync [::impl/clear-taps])
  (set! d/*debug* debug-tap)
  (->> (upsert-loader-el!)
       (dom/render [debug-marks])))
