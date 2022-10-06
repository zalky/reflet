(ns reflet.client.ui
  (:require [reactstrap-cljs.core :as b]
            [reagent.core :as r]
            [reflet.core :as f]
            [reflet.db :as db]
            [reflet.interop :as i]))

;; Re-frame/Clojurescript

(f/reg-event-db ::init
  (fn [db [_ {:player/keys [self]}]]
    (->> {:component/uuid (second self)
          :player/track   "/audio/01 Orbits.mp3"
          :player/state   ::paused}
         (db/mergen db))))

(f/reg-pull ::component
  (fn [self]
    [[:player/track
      :player/state]
     self]))

(f/reg-event-db ::toggle
  (fn [db [_ {:player/keys [self]}]]
    (db/update-inn db [self :player/state]
      (fn [state]
        (case state
          ::playing ::paused
          ::paused  ::playing)))))

;; Interop

(defn init-context
  [{node-r    :player/node
    context-r :player/context
    source-r  :player/source}]
  (let [context (js/AudioContext.)
        node    (i/get-obj @i/dom-db node-r)
        source  (.createMediaElementSource context node)]
    (.connect source (.-destination context))
    (i/reg-js context-r context)
    (i/reg-js source-r source)))

(defn update-context
  [{state     :player/state
    context-r :player/context
    node-r    :player/node
    :as       props}]
  (let [node    (i/get-obj @i/dom-db node-r)
        context (i/get-obj @i/js-db context-r)]
    (when-not context (init-context props))
    (case state
      ::paused  (.play node)
      ::playing (.pause node))))

(defn player-inner
  [_]
  (r/create-class
   {:component-did-update
    (f/props-did-update-handler update-context)

    :reagent-render
    (fn [{:player/keys [node track]}]
      [:audio {:ref (i/node! node)
               :src track}])}))

;; View

(defn info
  [{:player/keys [self]}]
  (r/with-let [c (f/subscribe [::component self])]
    [:div {:class "player-info mb-2"}
     (:player/track @c)]))

(defn player
  [props]
  (f/with-ref {:component/uuid [player/self]
               :js/uuid        [player/context player/source]
               :dom/uuid       [player/node]
               :in             props}
    (r/with-let [_      (f/dispatch [::init props])
                 toggle #(f/dispatch [::toggle props])
                 c      (f/subscribe [::component self])]
      (when-let [{:player/keys [state]} @c]
        [:div
         [info props]
         [player-inner (merge props @c)]
         [b/button-toolbar
          [b/button-group
           [b/button {:color    :primary
                      :on-click toggle}
            (case state
              ::playing "Pause"
              ::paused  "Play")]]]]))))

