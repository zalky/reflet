(ns reflet.log
  "Provides loggging functionality via timbre."
  (:require [clojure.string :as str]
            [reflet.core :as f]
            [taoensso.timbre :as log]))

;; Prevent handler overwriting warnings during cljs reload.
(f/set-loggers!
 {:warn (fn [& args]
          (when-not (re-find #"^re-frame: overwriting" (first args))
            (apply js/console.warn args)))})

(def log-middleware
  [#(assoc % :raw-console? true)])

(def re-frame-log-middleware
  "Workaround so we don't print an actual vector of args to the
  console. Instead, pass in the vector of args, and take first with
  timbre middleware."
  (cons #(update % :vargs first) log-middleware))

(defn re-frame-output-fn
  [{:keys [msg_ level ?err]}]
  (str (str/upper-case (name level)) ":" (force msg_)
       (when ?err
         (str "\n" (log/stacktrace ?err {})))))

(f/reg-fx :log
  (fn [[level & vargs :as arg]]
    (when arg
      (log/with-merged-config
        {:middleware re-frame-log-middleware
         :output-fn re-frame-output-fn}
        (log/log level vargs)))))

(def config
  "Basic timbre config."
  {:level :info
   :ns-whitelist ["reflet.*"]
   :middleware log-middleware
   :appenders {:console (log/console-appender)}})

(log/set-config! config)
