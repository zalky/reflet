(ns reflet.client.ui
  (:require [reactstrap-cljs.core :as b]
            [reagent.core :as r]
            [reflet.client.ui.impl :as impl]
            [reflet.client.ui.interop :as interop]
            [reflet.core :as f]
            [reflet.interop :as i]))

(defn dispatch-player-fsm
  [{:player/keys [self self-sel]}]
  @(f/subscribe [::impl/player-fsm self self-sel]))

(defmulti controls
  dispatch-player-fsm
  :hierarchy #'impl/state-hierarchy)

(defmethod controls ::impl/loading
  [_]
  [b/button {:color :primary}
   "Player"])

(defmethod controls ::impl/running
  [{:player/keys [self self-sel]}]
  (r/with-let [toggle #(f/dispatch [::impl/toggle self])
               state  (f/subscribe [::impl/player-fsm self self-sel])]
    [b/button {:color    :primary
               :on-click toggle}
     (case @state
       ::impl/playing "Pause"
       "Play")]))

(defn selector
  [{:player/keys [self self-sel]}]
  (r/with-let [on-click #(f/dispatch [::impl/toggle self-sel])
               info     (f/subscribe [::impl/track-info self])]
    [b/button {:color    :secondary
               :on-click on-click}
     (:kr.track/name @info "Select Track")]))

(defn track-list
  [{:player/keys [self self-sel]}]
  (r/with-let [tracks (f/subscribe [::impl/track-list])]
    [:div {:class "player-track-list mt-2"}
     (doall
      (for [{id     :system/uuid
             artist :kr.track/artist
             name   :kr.track/name
             :as    t} @tracks]
        (let [ref      [:system/uuid id]
              duration @(f/subscribe [::impl/track-duration ref])
              on-click #(f/dispatch [::impl/selected self-sel self ref])]
          ^{:key id}
          [:div {:on-click on-click}
           [:div artist]
           [:div name]
           [:div duration]])))]))

(defn player-inner
  [_]
  (r/create-class
   {:component-did-update
    (f/props-did-update-handler interop/update-context)

    :reagent-render
    (fn [{{uri :kr.track/uri} :player/track
          node                :player/node}]
      [:audio {:ref (i/node! node)
               :src uri}])}))

(defn player
  [props]
  (f/with-ref {:component/uuid [player/self player/self-sel]
               :js/uuid        [player/context player/source]
               :dom/uuid       [player/node]
               :in             props}
    (r/with-let [m          (f/subscribe [::impl/materialized self])
                 selecting? (f/subscribe [::impl/selecting? self-sel])]
      [:div {:class ["player" (when @selecting? "selecting")]}
       [player-inner (merge props @m)]
       [b/button-toolbar 
        [b/button-group [controls props]]
        [b/button-group [selector props]]]
       [track-list props]])))

