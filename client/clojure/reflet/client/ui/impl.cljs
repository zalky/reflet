(ns reflet.client.ui.impl
  (:require [reflet.core :as f]))

(f/reg-sub ::view
  (fn [db _]
    (get db ::view)))

(f/reg-event-db ::set-view
  (fn [db [_ view-id]]
    (assoc db ::view view-id)))
