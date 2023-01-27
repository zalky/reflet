(ns reflet.client.boot
  (:require [re-frame.core :as f*]
            [reflet.core :as f]
            [reflet.db :as db]
            [reflet.db.tx :as tx]))

(defn- add-id
  [m]
  (apply assoc m (db/random-ref :system/uuid)))

(defn- entities-tx
   [davis shorter album tracks]
   [{:system/uuid (second davis)
     :kr/name     "Miles Davis"}
    {:system/uuid (second shorter)
     :kr/name     "Wayne Shorter"}
    {:system/uuid     (second album)
     :kr/name         "Miles Smiles"
     :kr.album/artist davis
     :kr.album/tracks tracks}])

(defn- tracks-tx
  [davis shorter album]
  (->> [{:kr/name           "Orbits"
         :kr.track/artist   shorter
         :kr.track/album    album
         :kr.track/duration 281
         :kr/uri            "/audio/DbMaj7b5.mp3"}
        {:kr/name           "Circle"
         :kr.track/artist   davis
         :kr.track/album    album
         :kr.track/duration 352
         :kr/uri            "/audio/C7b9.mp3"}
        {:kr/name           "Footprints"
         :kr.track/artist   shorter
         :kr.track/album    album
         :kr.track/duration 586
         :kr/uri            "/audio/AbmMaj11.mp3"}
        {:kr/name           "Dolores"
         :kr.track/artist   shorter
         :kr.track/album    album
         :kr.track/duration 380
         :kr/uri            "/audio/BMaj7b5.mp3"}
        {:kr/name           "Freedom Jazz Dance"
         :kr.track/artist   {:kr/name "Eddie Harris"}
         :kr.track/album    album
         :kr.track/duration 433
         :kr/uri            "/audio/Fm11.mp3"}
        {:kr/name           "Ginger Bread Boy"
         :kr.track/artist   {:kr/name "Jimmy Heath"}
         :kr.track/album    album
         :kr.track/duration 433
         :kr/uri            "/audio/DbMaj7b5.mp3"}]
       (tx/walk-maps add-id)))

(f*/reg-cofx ::tracks
  (fn [cofx]
    (let [davis    (db/random-ref :system/uuid)
          shorter  (db/random-ref :system/uuid)
          album    (db/random-ref :system/uuid)
          tracks   (tracks-tx davis shorter album)
          entities (entities-tx davis shorter album tracks)]
      (assoc cofx
             ::tracks tracks
             ::entities entities))))

(f/reg-event-fx ::init-data
  (f*/inject-cofx ::tracks)
  (fn [{db       :db
        tracks   ::tracks
        entities ::entities} _]
    {:db (-> db
             (db/assocn :user/track-list tracks)
             (db/mergen entities))}))
