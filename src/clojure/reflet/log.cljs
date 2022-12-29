(ns reflet.log
  "Provides loggging functionality via timbre."
  (:require [clojure.string :as str]
            [re-frame.core :as f]
            [re-frame.loggers :as log]))

(f/reg-fx ::log
  (fn [[level & vargs :as arg]]
    (when arg
      (log/console level vargs))))

