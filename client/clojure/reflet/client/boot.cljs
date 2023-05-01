(ns reflet.client.boot
  (:require [re-frame.core :as f*]
            [reflet.core :as f]
            [reflet.db :as db]
            [reflet.db.tx :as tx]))

(defn- add-id
  [m]
  (apply assoc m (db/random-ref :system/uuid)))

(defn- musicians-tx
   [davis shorter album tracks]
  [{:system/uuid     (second davis)
    :kr/type         :kr.type/artist
    :kr.artist/label "Bluenote"
    :kr/name         "Miles Davis"}
   {:system/uuid (second shorter)
    :kr/type     :kr.type/artist
    :kr/name     "Wayne Shorter"}
   {:system/uuid     (second album)
    :kr/type         :kr.type/album
    :kr/name         "Miles Smiles"
    :kr.album/artist davis
    :kr.album/tracks tracks}])

(defn- tracks-tx
  [davis shorter album]
  (->> [{:kr/type           :kr.type/track
         :kr/name           "Orbits"
         :kr.track/artist   shorter
         :kr.track/album    album
         :kr.track/duration 281
         :kr/uri            "/audio/DbMaj7b5.mp3"}
        {:kr/type           :kr.type/track
         :kr/name           "Circle"
         :kr.track/artist   davis
         :kr.track/album    album
         :kr.track/duration 352
         :kr/uri            "/audio/C7b9.mp3"}
        {:kr/type           :kr.type/track
         :kr/name           "Footprints"
         :kr.track/artist   shorter
         :kr.track/album    album
         :kr.track/duration 586
         :kr/uri            "/audio/AbmMaj11.mp3"}
        {:kr/type           :kr.type/track
         :kr/name           "Dolores"
         :kr.track/artist   shorter
         :kr.track/album    album
         :kr.track/duration 380
         :kr/uri            "/audio/BMaj7b5.mp3"}
        {:kr/type           :kr.type/track
         :kr/name           "Freedom Jazz Dance"
         :kr.track/artist   {:kr/name "Eddie Harris"}
         :kr.track/album    album
         :kr.track/duration 433
         :kr/uri            "/audio/Fm11.mp3"}
        {:kr/type           :kr.type/track
         :kr/name           "Ginger Bread Boy"
         :kr.track/artist   {:kr/name "Jimmy Heath"}
         :kr.track/album    album
         :kr.track/duration 433
         :kr/uri            "/audio/DbMaj7b5.mp3"}]
       (tx/walk-maps add-id)))

(f*/reg-cofx ::remote-music-response
  ;; Stub out remote response.
  (fn [cofx]
    (let [d        (db/random-ref :system/uuid)
          s        (db/random-ref :system/uuid)
          a        (db/random-ref :system/uuid)
          tracks   (tracks-tx d s a)
          entities (musicians-tx d s a tracks)]
      (->> {::track-list tracks
            ::entities   entities}
           (assoc cofx ::response)))))

(f/reg-event-fx ::init-music-data
  (f*/inject-cofx ::remote-music-response)
  (fn [{db             :db
        {t ::track-list
         e ::entities} ::response} _]
    {:db (-> db
             (db/assocn :player/track-list t)
             (db/mergen e))}))

(defn- events-tx
  []
  (->> [{:kr/type  :kr.type/conference
         :kr/name  "Clojure Conj 2023"
         :kr/about "A conference for Clojure developers, companies and enthusiasts."
         :kr/city  "Durham, NC"
         :kr/topic {:kr/type        :kr.type/language
                    :kr/name        "Clojure"
                    :kr/description "Clojure is a dynamic and functional dialect of Lisp on the Java platform."}
         :kr/time  {:kr/type       :kr.type/time
                    :kr.time/start "04/27/2023"
                    :kr.time/end   "04/28/2023"}}
        {:kr/type        :kr.type/lecture
         :kr/name        "The Unanswered Question"
         :kr/description "A series of lectures given by Leonard Bernstein in the fall of 1973."
         :kr/institution "Harvard University"
         :kr/time        {:kr/type       :kr.type/time
                          :kr.time/start "09/01/1973"
                          :kr.time/end   "12/01/1973"}
         :kr/speaker     {:kr/type       :kr.type/conductor
                          :kr/name       "Leonard Bernstein"
                          :kr/occupation "Conductor"}}
        {:kr/type       :kr.type/performance
         :kr/name       "Quirky"
         :kr/location   "Brixton"
         :kr/time       {:kr/type       :kr.type/time
                         :kr.time/start "16/06/1994"}
         :kr/performers {:kr/type        :kr.type/music-group
                         :kr/name        "Autechre"
                         :kr/media       "Electronic music"
                         :kr/music-label "Warp"
                         :kr/members     [{:kr/type :kr.type/person
                                           :kr/name "Sean Booth"}
                                          {:kr/type :kr.type/person
                                           :kr/name "Robert Brown"}]}}]
       (tx/walk-maps add-id)))

(f*/reg-cofx ::remote-event-response
  ;; Stub out remote response.
  (fn [cofx]
    (assoc cofx ::response (events-tx))))

(f/reg-event-fx ::init-event-data
  (f*/inject-cofx ::remote-event-response)
  (fn [{db     :db
        events ::response} _]
    {:db (db/assocn db :desc/event-list events)}))

(f/reg-event-fx ::init-data
  (fn [_ _]
    {:dispatch-n [[::init-music-data]
                  [::init-event-data]]}))
