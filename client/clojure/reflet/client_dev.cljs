(ns reflet.client-dev
  (:require [reflet.client :as client]
            [reflet.client.boot :as boot]
            [reflet.core :as f]
            [reflet.debug.ui :as debug]))

(defn ^:dev/after-load render!
  []
  (client/render!)
  (debug/render!))     ; Must happen after f/clear-subscription-cache!

(defn init!
  []
  (f/disp-sync [::boot/boot])
  (render!))
