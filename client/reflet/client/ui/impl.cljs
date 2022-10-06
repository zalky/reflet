(ns reflet.client.ui.impl
  (:require [cinch.core :as util]
            [reflet.core :as f]
            [reflet.fsm :as fsm]
            [reflet.db :as db]))

(def state-hierarchy
  (util/derive-pairs
   [::init               ::loading
    [::paused ::playing] ::running]))

(fsm/reg-fsm ::selector-fsm
  (fn [self-sel]
    {:id         self-sel
     :state-attr :player/selector-state
     :start      ::closed
     :fsm        {::closed {[::toggle self-sel] {:to ::open}}
                  ::open   {[::toggle self-sel]   {:to ::closed}
                            [::selected self-sel] {:to ::closed}}}}))

(fsm/reg-fsm ::player-fsm
  ;; We need a separate starting state, in addition to ::paused
  ;; and ::playing. This true both because we do not want to start
  ;; playing if we have not selected a track, but also the browser
  ;; will complain if we try to mutate the AudioContext object before
  ;; a user event has been triggered.
  (fn [self self-sel]
    {:id         self
     :state-attr :player/state
     :start      ::init
     :fsm        {::init    {[::selected self-sel] {:to ::playing}}
                  ::playing {[::toggle self] {:to ::paused}}
                  ::paused  {[::toggle self]       {:to ::playing}
                             [::selected self-sel] {:to ::playing}}}}))

(f/reg-no-op ::toggle)

(f/reg-event-db ::selected
  (fn [db [_ _ self track-r]]
    (db/assoc-inn db [self :player/track] track-r)))

(f/reg-pull ::track-list
  (fn []
    [{:user/track-list
      [:system/uuid
       :kr.track/artist
       :kr.track/name]}]))

(f/reg-pull ::materialized
  (fn [self]
    [[:player/state
      {:player/track [:kr.track/uri]}]
     self]))

(f/reg-pull ::track-info
  (fn [self]
    [{:player/track
      [:system/uuid
       :kr.track/name]}
     self]))

(f/reg-pull ::track-duration
  (fn [id]
    [:kr.track/duration id])
  (fn [secs]
    (when (number? secs)
      (str (Math/floor (/ secs 60)) ":" (mod secs 60)))))

(f/reg-sub ::selecting?
  (fn [[_ self-sel]]
    (f/subscribe [::selector-fsm self-sel]))
  (fn [state _]
    (= state ::open)))
