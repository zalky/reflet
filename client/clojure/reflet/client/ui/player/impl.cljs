(ns reflet.client.ui.player.impl
  (:require [cinch.core :as util]
            [reflet.core :as f]
            [reflet.fsm :as fsm]
            [reflet.db :as db]))

(def state-hierarchy
  (util/derive-pairs
   [[::paused ::playing] ::ready]))

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
  ;; browser will complain (by design) if we try to mutate the
  ;; AudioContext object before a user event has been triggered.
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
       :kr/name
       {:kr.track/artist [:kr/name]}]}]))

(f/reg-pull ::materialized
  (fn [self]
    [[:player/state
      {:player/track [:kr/uri]}]
     self]))

(f/reg-pull ::track-info
  (fn [self]
    [{:player/track
      [:system/uuid
       {:kr.track/artist [:kr/name]}
       :kr/name]}
     self])
  (fn [{a   :kr.track/artist
        n   :kr/name
        :as track}]
    (when track
      (str (:kr/name a) " - " n))))

(f/reg-pull ::track-duration
  (fn [ref]
    [:kr.track/duration ref])
  (fn [secs]
    ;; Convert numeric seconds to a HH:mm:ss string representation
    (when (number? secs)
      (->> (-> secs
               (* 1000)
               ( js/Date.)
               (.toISOString)
               (.slice 11 19))
           (drop-while #{\0 \:})
           (apply str)))))

(f/reg-sub ::selecting?
  (fn [[_ self]]
    (f/sub [::selector-fsm self]))
  (fn [state _]
    (= state ::open)))
