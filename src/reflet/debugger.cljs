(ns reflet.debugger
  (:require [react-dom :as react-dom]
            [reagent.core :as r]
            [reflet.core :as f]))

(defn debug-node
  [el]
  (let [r (.getBoundingClientRect el)]
    [:div {:class "debug-node"
           :style {:left (.-left r)
                   :top  (.-top r)}}]))

(defn- body-el
  []
  (.querySelector js/document "body"))

(defn- tap-sibling
  [node el]
  (some->> el
           (.-nextSibling)
           (reset! node)))

(defn debugger
  []
  (r/with-let [node (r/atom nil)
               body (body-el)]
    (if-not @node
      [:div {:class "debug-tap-sibling"
             :ref   (partial tap-sibling node)}]
      (-> (debug-node @node)
          (r/as-element)
          (react-dom/createPortal body)))))

(f/reg-event-fx ::set
  (fn [db [_ debugger]]
    (reset! f/debugger debugger)
    nil))
