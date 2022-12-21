(ns reflet.debug.ui.data.impl
  (:require [reflet.core :as f]
            [reflet.db :as db]))

(f/reg-pull ::entity
  (fn [ref]
    [['*] ref]))

(f/reg-pull ::expand?
  (fn [self]
    [::expand self]))

(f/reg-event-db ::toggle-expand
  (fn [db [_ self]]
    (db/update-inn db [self ::expand] not)))
