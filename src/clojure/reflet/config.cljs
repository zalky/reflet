(ns reflet.config
  "Registers reflet configuration map. The configuration should be
  registered before application boot. Currently supported
  configuration options:

  :id-attrs
            A set of unique id attributes that configure the db
            normalized data model.

  :dispatch
            An initial dispatch immediately after configuration.

  :pull-fn
            Overrides the default pull implementation. This fn must
            fullfill the input and ouput contract specified by
            reflet.db/default-pull-impl. See the function's doc
            string for more info.

  :trace-queue-size Sets the trace queue size for the debugger panels.
  Default is 50."
  (:require [re-frame.core :as f]))

(def ^:private config
  (atom {}))

(f/reg-fx ::config
  (partial reset! config))

(defn get-config
  []
  @config)
