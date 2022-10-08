(ns reflet.client.ui.interop
  (:require [reflet.client.ui.impl :as impl]
            [reflet.interop :as i]))

(defn create-context
  "Idempotent constructor."
  [{node-r    :player/node
    context-r :player/context
    source-r  :player/source}]
  (when-not (i/grab context-r)
    (let [context (js/AudioContext.)
          node    (i/grab node-r)
          source  (.createMediaElementSource context node)]
      (.connect source (.-destination context))
      (i/reg context-r context)
      (i/reg source-r source))))

(defn running?
  [state]
  (isa? impl/state-hierarchy state ::impl/running))

(defn update-context
  "Syncs the mutable state of the audio element given immutable app
  state. Checks that we have recieved user input, via
  the :player/state. The browser will complain if we start an audio
  context without first having had user input."
  [_ {state  :player/state
      node-r :player/node
      :as    props}]
  (when (running? state)
    (let [node (i/grab node-r)]
      (create-context props)
      (case state
        ::impl/playing (.play node)
        ::impl/paused  (.pause node)))))
