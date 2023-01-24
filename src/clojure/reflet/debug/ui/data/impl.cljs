(ns reflet.debug.ui.data.impl
  (:require [reflet.core :as f]
            [reflet.db :as db]
            [reflet.debug.ui.impl :as ui]
            [reflet.interop :as i]))

(f/reg-pull ::entity
  (fn [ref]
    [['*] ref]))

(f/reg-pull ::expand?
  (fn [self]
    [::expand self]))

(f/reg-event-db ::toggle-expand
  (fn [db [_ self]]
    (db/update-inn db [self ::expand] not)))

(def dynamic-flex-shrink-factor
  30)

(defn coll-item-min-width
  "Minimum width for collection elements."
  [el el-width]
  (let [p  (.-parentElement el)
        pw (ui/px p :width)]
    (->> dynamic-flex-shrink-factor
         (/ (- pw ui/new-panel-width))
         (max 0)
         (+ 40)
         (min el-width))))

(defn- coll-item-width
  [^js el]
  (let [w  (ui/px el :width)
        mw (coll-item-min-width el w)]
    (.log js/console el w mw (js/Math.sqrt (/ w mw)))
    {:min-width   (if (< mw w) mw w)
     :flex-shrink (js/Math.sqrt (/ w mw))}))

(f/reg-sub ::coll-item-style
  (fn [[_ el-r]]
    (f/sub [::i/grab el-r]))
  (fn [el _]
    (when el
      (coll-item-width el))))
