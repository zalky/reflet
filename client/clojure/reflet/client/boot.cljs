(ns reflet.client.boot
  (:require [reflet.core :as f]
            [reflet.db :as db]
            [reflet.db.tx :as tx]))

(def track-list
  [{:kr.track/name     "Orbits"
    :kr.track/artist   "Miles Davis"
    :kr.track/album    "Miles Smiles"
    :kr.track/duration 281
    :kr.track/uri      "/audio/Miles Davis - Orbits.mp3"}
   {:kr.track/name     "Maleheya"
    :kr.track/artist   "RAMZi"
    :kr.track/album    "For Haku"
    :kr.track/duration 233
    :kr.track/uri      "/audio/RAMZi - Maleheya.mp3"}
   {:kr.track/name     "Berceuse in D-flat major, Op.57 (Michelangeli)"
    :kr.track/artist   "Chopin"
    :kr.track/duration 256
    :kr.track/uri      "/audio/Chopin - Berceuse in D-flat major, Op.57 (Michelangeli).mp3"}
   {:kr.track/name     "Sister Brother"
    :kr.track/artist   "FJ McMahon"
    :kr.track/duration 338
    :kr.track/uri      "/audio/FJ McMahon - Sister Brother.mp3"}
   {:kr.track/name     "Fooling You"
    :kr.track/artist   "Terry Reid"
    :kr.track/album    "Seed of Memory"
    :kr.track/duration 441
    :kr.track/uri      "/audio/Terry Reid - Fooling You.mp3"}])

(defn- add-id
  [m]
  (assoc m :system/uuid (random-uuid)))

(f/reg-event-db ::init-tracks
  (fn [db _]
    (->> track-list
         (tx/walk-maps add-id)
         (db/assocn db :user/track-list))))
