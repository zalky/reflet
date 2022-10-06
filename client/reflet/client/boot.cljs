(ns reflet.client.boot
  (:require [reflet.core :as f]
            [reflet.db :as db]))

(f/reg-event-fx ::boot
  (fn [_ _]
    {:db (db/new-db)}))
