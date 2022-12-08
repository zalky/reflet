(ns reflet.debug.preload
  (:require [reflet.db :as db]
            [reflet.debug.ui :as ui]))

(set! db/tap-fn ui/tap)
