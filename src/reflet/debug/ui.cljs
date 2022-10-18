(ns reflet.debug.ui
  (:require [react-dom :as react-dom]
            [reagent.core :as r]
            [reflet.core :as f]
            [reflet.debug.cluster :as c]
            [reflet.debug.glyphs :as g]
            [reflet.debug.ui.impl :as impl]
            [reflet.interop :as i]
            [reflet.debug :as d]))

(defn rect
  [el & [selectors]]
  (let [r (.getBoundingClientRect el)]
    {:top    (.-top r)
     :bottom (.-bottom r)
     :left   (.-left r)
     :right  (.-right r)
     :width  (.-width r)
     :height (.-height r)}))

(defn shift
  [target-rect node-r]
  (when-let [el (i/grab node-r)]
    (let [{l :left
           t :top}    target-rect
          {w :width
           h :height} (rect el)]
      {:left (max (- l w) 0)
       :top  (max (- t h) 0)})))

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
  [{:keys [refs]}]
  [:div {:class "debug-refs"}
   [debug-value refs]])

(defn- debug-panel
  [{:keys [id name] :as props}]
  (f/with-ref {:dom/uuid [debug/el]}
    (let [r (f/subscribe [::impl/rect id])]
      [:div {:ref   (i/el! el)
             :class "debug-panel"
             :style (shift @r node)}
       [:div {:class "debug-content"}
        [:div {:class "debug-header"} name]
        [debug-refs props]]
       [:div]])))

(defn- debug-node
  [{:keys [id]}]
  (f/with-ref {:dom/uuid [debug/el]}
    (let [r (f/subscribe [::impl/rect id])]
      [:div {:ref   (i/el! el)
             :class "debug-node group"
             :style (shift @r node)}
       [g/node-icon {:stack true}]])))

(defn- tap
  [id target tap-el]
  (some->> tap-el
           (.-nextSibling)
           (reset! target)
           (rect)
           (vector ::impl/tap id)
           (f/dispatch)))

(defn- body-el
  []
  (.querySelector js/document "body"))

(defn debug
  [{:keys [id refs] :as props}]
  (r/with-let [target (r/atom nil)
               body   (body-el)]
    (if-not @target
      [:div {:class "debug-tap"
             :ref   (partial tap id target)}]
      (binding [d/*debug* false]
        (-> [(if (contains? refs :player/self)
               debug-panel
               debug-node) props]
            (r/as-element)
            (react-dom/createPortal body))))))

(f/reg-event-fx ::activate
  (fn [db [_ debug-fn]]
    (set! d/*debug* debug-fn)
    nil))
