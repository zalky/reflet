(ns reflet.debugger
  (:require [react-dom :as react-dom]
            [reagent.core :as r]
            [reflet.core :as f]
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
  [target-el node-r]
  (when-let [el (i/grab node-r)]
    (let [{l :left
           t :top}    (rect target-el)
          {w :width
           h :height} (rect el)]
      {:left (max (- l (/ w 2)) 0)
       :top  (max (- t (/ h 2)) 0)})))

(defn- debug-node
  [target-el refs]
  (with-ref {:dom/uuid [debug/node]}
    [:div {:ref   (i/node node)
           :class "debug-node"
           :style (shift target-el node)}]))

(defn- body-el
  []
  (.querySelector js/document "body"))

(defn- tap
  [target tap-el]
  (some->> tap-el
           (.-nextSibling)
           (reset! target)))

(defn debugger
  [refs]
  (r/with-let [target (r/atom nil)
               body   (body-el)]
    (if-not @target
      [:div {:class "debug-tap"
             :ref   (partial tap target)}]
      (-> (debug-node @target refs)
          (r/as-element)
          (react-dom/createPortal body)))))

(f/reg-event-fx ::set
  (fn [db [_ debugger]]
    (reset! f/debugger debugger)
    nil))
