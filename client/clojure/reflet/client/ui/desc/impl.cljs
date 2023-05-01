(ns reflet.client.ui.desc.impl
  (:require [reflet.core :as f]
            [reflet.db :as db]))

(def hierarchy
  [[:kr.type/language
    :kr.type/music-group
    :kr.type/person] :kr.type/entity
   :kr.type/conductor :kr.type/person
   [:kr.type/lecture
    :kr.type/conference
    :kr.type/performance] :kr.type/event])

;; Condensed representations

(f/reg-desc [::condensed :kr.type/entity]
  [:kr/name])

(f/reg-desc [::condensed :kr.type/time]
  [:kr.time/start
   :kr.time/end])

(f/reg-desc [::condensed :kr.type/event]
  [:kr/name
   {:kr/time ::condensed}])

;; Detailed representations

(f/reg-desc [::detailed :kr.type/entity]
  [:kr/name
   :kr/description])

(f/reg-desc [::detailed :kr.type/conference]
  [:kr/name
   :kr/about
   :kr/city
   {:kr/time ::condensed}
   {:kr/topic ::detailed}])

(f/reg-desc [::detailed :kr.type/lecture]
  [:kr/name
   :kr/description
   {:kr/time ::condensed}
   {:kr/speaker ::detailed}])

(f/reg-desc [::detailed :kr.type/performance]
  [:kr/name
   :kr/location
   {:kr/time ::condensed}
   {:kr/performers ::detailed}])

(f/reg-desc [::detailed :kr.type/person]
  [:kr/name
   :kr/occupation])

(f/reg-desc [::detailed :kr.type/music-group]
  [:kr/name
   :kr/description
   :kr/media
   :kr/music-label
   {:kr/members ::condensed}])

(f/reg-pull ::selected-context
  (fn [self]
    [::selected-context self])
  (fn [context]
    (or context ::condensed)))

(f/reg-event-db ::select-context
  (fn [db [_ self context]]
    (db/assoc-inn db [self ::selected-context] context)))
