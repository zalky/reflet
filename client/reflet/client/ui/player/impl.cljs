(ns reflet.client.ui.player.impl
  (:require [cinch.core :as util]
            [reflet.core :as f]
            [reflet.fsm :as fsm]
            [reflet.db :as db]))

(def state-hierarchy
  (util/derive-pairs
   [[::paused ::playing] ::running]))

(fsm/reg-fsm ::selector-fsm
  (fn [self]
    {:ref  self
     :attr :player/selector-state
     :fsm  {nil    {[::toggle self] ::open}
            ::open {[::toggle self]   nil
                    [::selected self] nil}}}))

(fsm/reg-fsm ::player-fsm
  ;; We need a separate starting state in addition to ::paused
  ;; and ::playing. This is true both because we do not want to start
  ;; playing if a track has not been selected, but also because the
  ;; browser will complain if we try to mutate the AudioContext object
  ;; before a user event has been triggered.
  (fn [self]
    {:ref  self
     :attr :player/state
     :fsm  {nil       {[::selected self] ::playing}
            ::playing {[::pause self] ::paused}
            ::paused  {[::play self]     ::playing
                       [::selected self] ::playing}}}))

(f/reg-no-op ::toggle ::play ::pause)

(f/reg-event-db ::selected
  (fn [db [_ self track-r]]
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
      (str (quot secs 60) ":" (mod secs 60)))))

(f/reg-sub ::selecting?
  (fn [[_ self]]
    (f/sub [::selector-fsm self]))
  (fn [state _]
    (= state ::open)))
