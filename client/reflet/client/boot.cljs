(ns reflet.client.boot
  (:require [reflet.core :as f]
            [reflet.db :as db]))

(def track-list
  [{:system/uuid       (random-uuid)
    :kr.track/name     "Orbits"
    :kr.track/artist   "Miles Davis"
    :kr.track/album    "Miles Smiles"
    :kr.track/duration 281
    :kr.track/uri      "/audio/Miles Davis - Orbits.mp3"}
   {:system/uuid       (random-uuid)
    :kr.track/name     "Maleheya"
    :kr.track/artist   "RAMZi"
    :kr.track/album    "For Haku"
    :kr.track/duration 233
    :kr.track/uri      "/audio/RAMZi - Maleheya.mp3"}
   {:system/uuid       (random-uuid)
    :kr.track/name     "Berceuse in D-flat major, Op.57 (Michelangeli)"
    :kr.track/artist   "Chopin"
    :kr.track/duration 256
    :kr.track/uri      "/audio/Chopin - Berceuse in D-flat major, Op.57 (Michelangeli).mp3"}
   {:system/uuid       (random-uuid)
    :kr.track/name     "Sister Brother"
    :kr.track/artist   "FJ McMahon"
    :kr.track/duration 338
    :kr.track/uri      "/audio/FJ McMahon - Sister Brother.mp3"}
   {:system/uuid       (random-uuid)
    :kr.track/name     "Fooling You"
    :kr.track/artist   "Terry Reid"
    :kr.track/album    "Seed of Memory"
    :kr.track/duration 441
    :kr.track/uri      "/audio/Terry Reid - Fooling You.mp3"}])

(f/reg-event-fx ::boot
  (fn [_ _]
    {:db (-> (db/new-db)
             (db/assocn :user/track-list track-list))}))
