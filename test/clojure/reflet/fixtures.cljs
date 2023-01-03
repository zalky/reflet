(ns reflet.fixtures
  (:require [reagent.core :as r]
            [reflet.core :as f]
            [reflet.db :as db]
            [reflet.interop :as i])
  (:require-macros reflet.fixtures))

(def fake-ratom-generation
  "Used by fake-reactive-context, only during testing."
  (atom 0))

(defn base-fixtures
  "These are included by default by the
  `reflet.fixtures/run-test-sync` macro."
  [f]
  (with-redefs [db/query-index     (r/atom {})
                db/mounted-refs    (r/atom #{})
                i/db               (r/atom {})
                f/debounced-events (r/atom {})]
    (f/disp-sync [::f/config])
    (f)))
