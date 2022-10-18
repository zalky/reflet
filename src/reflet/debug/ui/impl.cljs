(ns reflet.debug.ui.impl
  (:require [cinch.core :as util]
            [reflet.core :as f]
            [reflet.db :as db]
            [reflet.fsm :as fsm]
            [reflet.interop :as i]))

(defn rect
  [el & [selectors]]
  (let [r (.getBoundingClientRect el)]
    {:top    (.-top r)
     :bottom (.-bottom r)
     :left   (.-left r)
     :right  (.-right r)
     :width  (.-width r)
     :height (.-height r)}))

(defn position
  [el target-rect]
  (when el
    (let [{l :left
           t :top}    target-rect
          {w :width
           h :height} (rect el)]
      {:left (max (- l w) 0)
       :top  (max (- t h) 0)})))

(f/reg-event-db ::init-node
  (fn [db [_ id el]]
    (->> (db/get-inn db [id :debug/rect])
         (position el)
         (db/assoc-inn db [id :debug.node/rect]))))

(f/reg-event-db ::init-panel
  (fn [db [_ id el]]
    (->> (db/get-inn db [id :debug/rect])
         (position el)
         (db/assoc-inn db [id :debug.panel/rect]))))

(f/reg-pull ::node-geo
  (fn [id]
    [:debug.node/rect id])
  (fn [rect]
    (select-keys rect [:left :top])))

(f/reg-pull ::panel-geo
  (fn [id]
    [:debug.panel/rect id])
  (fn [rect]
    (select-keys rect [:left :top])))

(fsm/reg-fsm ::panel
  (fn [id]
    {:ref  id
     :attr :debug.panel/state
     :fsm  {nil    {[::toggle id] ::open}
            ::open {[::toggle id] nil}}}))

(f/reg-no-op ::toggle)

(f/reg-event-db ::tap
  (fn [db [_ props r]]
    (-> db
        (update ::tapped util/conjs props)
        (db/mergen props))))

(f/reg-sub ::tapped
  (fn [db _]
    (get db ::tapped)))

;;;; Dragging

(defn prop
  [x]
  (cond
    (keyword? x) (name x)
    (string? x)  x))

(defn set-style
  [el m]
  (let [style (.-style el)]
    (doseq [[k v] m]
      (.setProperty style (prop k) v))
    el))

(f/reg-sub ::dragging
  (fn [db _]
    (get db ::dragging)))

(defn viewport-size
  []
  (let [el (.-documentElement js/document)]
    {:width  (max (or (.-clientWidth el) 0)
                  (or (.-innerWidth js/window) 0))
     :height (max (or (.-clientHeight el) 0)
                  (or (.-innerHeight js/window) 0))}))

(f/reg-event-fx ::drag
  (fn [{db :db} [_ e-drag id dx dy w h]]
    (let [{vw :width
           vh :height} (viewport-size)

          x   (.-clientX e-drag)
          y   (.-clientY e-drag)
          l   (-> (max (- x dx) 0)
                  (min (- vw w)))
          t   (-> (max (- y dy) 0)
                  (min (- vh h)))
          pos {:left l :top t}]
      {:db (db/update-inn db [id :debug.panel/rect] merge pos)})))

(defn drag-listener!
  [e-mouse-down db id]
  (let [{l :left
         t :top
         w :width
         h :height} (db/get-inn db [id :debug.panel/rect])

        x  (.-clientX e-mouse-down)
        y  (.-clientY e-mouse-down)
        dx (- x l)
        dy (- y t)]
    (.preventDefault e-mouse-down)
    (fn [e-drag]
      (.preventDefault e-drag)
      (f/dispatch-sync [::drag e-drag id dx dy w h]))))

(defn drag-unlistener!
  [listener]
  (fn anon [_]
    (.removeEventListener js/document "mousemove" listener)
    (.removeEventListener js/document "mouseup" anon)
    (f/dispatch-sync [::drag-stop!])))

(f/reg-event-fx ::drag!
  (fn [{db :db} [_ e-mouse-down id]]
    (let [listener   (drag-listener! e-mouse-down db id)
          unlistener (drag-unlistener! listener)]
      (.addEventListener js/document "mousemove" listener)
      (.addEventListener js/document "mouseup" unlistener)
      {:db (assoc db ::dragging true)})))

(f/reg-event-db ::drag-stop!
  (fn [db _]
    (assoc db ::dragging false)))
