(ns reflet.debug.ui.data.impl
  (:require [reflet.core :as f]))

(f/reg-pull ::data
  (fn [ref]
    ['[*] ref]))
