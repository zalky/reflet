(ns reflet.debugger
  (:require [react-dom :as react-dom]
            [reagent.core :as r]
            [reflet.core :as f]
            [reflet.db :as db]
            [reflet.db.normalize :as norm]
            [reflet.debugger.cluster :as c]
            [reflet.debugger.glyphs :as g]
            [reflet.interop :as i])
  (:require-macros [reflet.debugger :refer [with-ref]]))

(defn- rect
  [el & [selectors]]
  (let [r (.getBoundingClientRect el)]
    {:top    (.-top r)
     :bottom (.-bottom r)
     :left   (.-left r)
     :right  (.-right r)
     :width  (.-width r)
     :height (.-height r)}))

(defn- shift
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
  [refs]
  [:div {:class "debug-refs"}
   [debug-value refs]])

(defn- debug-context
  [refs]
  [:div {:class "debug-map-data"}
    (map-indexed
     (fn [i [k ref]]
       [:div {:key i}
        [debug-value k]
        [debug-value ref]])
     refs)])

(f/reg-sub ::rect
  (fn [db [_ id]]
    (get-in db [::debugger id ::rect])))

(defn- debug-panel
  [id refs]
  (with-ref {:dom/uuid [debug/node]}
    (let [r (f/subscribe [::rect id])]
      [:div {:ref   (i/node! node)
             :class "debug-panel"
             :style (shift @r node)}
       [:div {:class "debug-content"}
        [debug-refs refs]
        [debug-context refs]]
       [:div]])))

(defn- debug-node
  [id refs]
  (with-ref {:dom/uuid [debug/node]}
    (let [r (f/subscribe [::rect id])]
      [:div {:ref   (i/node! node)
             :class "debug-node group"
             :style (shift @r node)}
       [g/node-icon {:stack true}]])))

(defn- body-el
  []
  (.querySelector js/document "body"))

(f/reg-event-db ::tap
  (fn [db [_ id r]]
    (assoc-in db [::debugger id ::rect] r)))

(defn- tap
  [id target tap-el]
  (some->> tap-el
           (.-nextSibling)
           (reset! target)
           (rect)
           (vector ::tap id)
           (f/dispatch)))

(defn debugger
  [id refs]
  (r/with-let [target (r/atom nil)
               body   (body-el)]
    (if-not @target
      [:div {:class "debug-tap"
             :ref   (partial tap id target)}]
      (-> [(if (contains? refs :player/self)
             debug-panel
             debug-node) id refs]
          (r/as-element)
          (react-dom/createPortal body)))))

(f/reg-event-fx ::set
  (fn [db [_ debugger]]
    (reset! db/debugger debugger)
    nil))
