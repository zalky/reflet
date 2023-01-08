(ns reflet.log
  (:require [re-frame.core :as f]
            [re-frame.loggers :as log]))

(f/reg-fx ::log
  (fn [[level & vargs :as arg]]
    (when arg
      (apply log/console level vargs))))
