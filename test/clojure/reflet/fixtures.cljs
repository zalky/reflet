(ns reflet.fixtures
  (:require [reagent.core :as r]
            [reflet.core :as f]
            [reflet.db :as db]
            [reflet.interop :as i])
  (:require-macros reflet.fixtures))

(defn base-fixtures
  "These are included by default by the
  `reflet.fixtures/run-test-sync` macro."
  [f]
  (with-redefs [db/query-index            (r/atom {})
                db/mounted-transient-refs (r/atom #{})
                i/db                      (r/atom {})
                f/debounced-events        (r/atom {})]
    (binding [f/*force-persistent-refs* true]
      (f/disp-sync [::f/config])
      (f))))
