(ns reflet.client.ui.player.interop
  (:require [reflet.client.ui.player.impl :as impl]
            [reflet.interop :as i]))

(defn create-context
  "Idempotent constructor."
  [{el-r      :player/el
    context-r :player/context
    source-r  :player/source}]
  (when-not (i/grab context-r)
    (let [context (js/AudioContext.)
          el      (i/grab el-r)
          source  (.createMediaElementSource context el)]
      (.connect source (.-destination context))
      (i/reg context-r context {:destroy #(.close context)})
      (i/reg source-r source))))

(defn ready?
  [state]
  (isa? impl/state-hierarchy state ::impl/ready))

(defn update-context
  "Syncs the mutable state of the audio element given immutable app
  state. Checks that we have received user input, via
  the :player/state. The browser will complain if we start an audio
  context without first having had user input."
  [_ {state :player/state
      el-r  :player/el
      :as   props}]
  (when (ready? state)
    (let [el (i/grab el-r)]
      (create-context props)
      (case state
        ::impl/playing (.play el)
        ::impl/paused  (.pause el)))))
