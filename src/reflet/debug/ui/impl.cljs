(ns reflet.debug.ui.impl
  (:require [reflet.core :as f]))

(f/reg-sub ::rect
  (fn [db [_ id]]
    (get-in db [::debug id ::rect])))

(f/reg-event-db ::tap
  (fn [db [_ id r]]
    (assoc-in db [::debug id ::rect] r)))
