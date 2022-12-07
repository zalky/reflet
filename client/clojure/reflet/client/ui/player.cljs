(ns reflet.client.ui.player
  (:require [reactstrap-cljs.core :as b]
            [reagent.core :as r]
            [reflet.client.ui.player.impl :as impl]
            [reflet.client.ui.player.interop :as interop]
            [reflet.core :as f]
            [reflet.interop :as i]))

(defn dispatch-player-fsm
  [{:player/keys [self]}]
  @(f/sub [::impl/player-fsm self]))

(defmulti controls
  dispatch-player-fsm
  :hierarchy #'impl/state-hierarchy)

(defmethod controls nil
  [_]
  [b/button {:color :primary}
   "Player"])

(defmethod controls ::impl/playing
  [{:player/keys [self]}]
  [b/button {:color    :primary
             :on-click #(f/disp [::impl/pause self])}
   "Pause"])

(defmethod controls ::impl/paused
  [{:player/keys [self]}]
  [b/button {:color    :primary
             :on-click #(f/disp [::impl/play self])}
   "Play"])

(defn selector
  [{:player/keys [self]}]
  (let [on-click #(f/disp [::impl/toggle self])
        info     (f/sub [::impl/track-info self])]
    [b/button {:color    :secondary
               :on-click on-click}
     (:kr.track/name @info "Select Track")]))

(defn track-list
  [{:player/keys [self]}]
  (let [tracks (f/sub [::impl/track-list])]
    [:div {:class "player-track-list mt-2"}
     (doall
      (for [{id     :system/uuid
             artist :kr.track/artist
             name   :kr.track/name} @tracks]
        (let [ref      [:system/uuid id]
              duration @(f/sub [::impl/track-duration ref])
              on-click #(f/disp [::impl/selected self ref])]
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
          el                  :player/el}]
      [:audio {:ref (i/el! el)
               :src uri}])}))

(defn player
  [props]
  (f/with-ref {:cmp/uuid [player/self]
               :js/uuid  [player/context player/source]
               :el/uuid  [player/el]
               :in       props}
    (let [m          (f/sub [::impl/materialized self])
          selecting? (f/sub [::impl/selecting? self])]
      [:div {:class ["player" (when @selecting? "selecting")]}
       [player-inner (merge props @m)]
       [b/button-toolbar
        [b/button-group [controls props]]
        [b/button-group [selector props]]]
       [track-list props]])))