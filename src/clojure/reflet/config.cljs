(ns reflet.config
  "Registers Reflet configuration map. The configuration should be
  registered before application boot. Currently supported
  configuration options:

  :id-attrs
            A set of unique id attributes that configure the db
            graph data model.

  :dispatch
            An initial dispatch immediately after configuration.

  :pull-fn
            Overrides the default pull implementation. This fn must
            fulfill the input and output contract specified by
            reflet.db/default-pull-impl. See the function's doc
            string for more info.

  :debug-hotkey
            A character literal for the key that when pressed toggles
            the debugger overlay. Default is \\j.

  :trace-queue-size
            Sets the trace queue size for the debugger panels.
            Default is 50."
  (:require [re-frame.core :as f]))

(def ^:private config
  (atom {}))

(f/reg-fx ::config
  (partial reset! config))

(defn get-config
  []
  @config)
