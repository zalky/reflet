(ns reflet.fixtures
  (:require [reagent.core :as r]
            [reflet.core :as f]
            [reflet.db :as db]
            [reflet.interop :as i]            
            [taoensso.timbre :as log]

            ;; Required for use.
            [reflet.log])
  (:require-macros reflet.fixtures))

(log/set-level! :warn)

(f/reg-event-fx ::internals
  (fn [_ _]
    {:db (db/new-db)}))

(defn base-fixtures
  "These are included by default by the
  `reflet.fixtures/run-test-sync` macro."
  [f]
  (with-redefs [db/query-index            (r/atom {})
                db/mounted-transient-refs (r/atom #{})
                i/db                      (r/atom {})
                f/debounced-events        (r/atom {})]
    (binding [f/*force-persistent-refs* true]
      (f/disp-sync [::internals])
      (f))))
