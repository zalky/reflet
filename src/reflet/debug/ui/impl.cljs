(ns reflet.debug.ui.impl
  (:require [cinch.core :as util]
            [reflet.core :as f]
            [reflet.db :as db]
            [reflet.debug :as d]
            [reflet.debug.cluster :as c]
            [reflet.fsm :as fsm]
            [reflet.interop :as i]))

(defn rect
  "Keep in mind that .getBoundingClientRect will return zeros during
  certain phases of the react lifecycle. Use it wisely."
  [el & [selectors]]
  (let [r (.getBoundingClientRect el)]
    {:top    (.-top r)
     :bottom (.-bottom r)
     :left   (.-left r)
     :right  (.-right r)
     :width  (.-width r)
     :height (.-height r)}))

(defn init-rect
  [el target-rect]
  (when el
    (let [{l :left
           t :top}    target-rect
          {w :width
           h :height} (rect el)]
      {:left   (max (- l w) 0)
       :top    (max (- t h) 0)
       :width  w
       :height h})))

(defn rect-attr
  [debug-type]
  (-> "debug."
      (str (name type))
      (keyword "rect")))

(f/reg-event-db ::set-props
  (fn [db [_ self props]]
    (as-> (apply assoc props self) %
      (dissoc % :debug/el)
      (db/mergen db %))))

(f/reg-event-db ::set-rect
  (fn [db [_ self tap el]]
    (->> (db/get-inn db [tap :debug/rect])
         (init-rect (i/grab el))
         (db/assoc-inn db [self :debug/rect]))))

(f/reg-event-db ::set-centroid
  (fn [db [_ self {:keys [x y]} el]]
    (->> {:left x :top y}
         (init-rect (i/grab el))
         (db/assoc-inn db [self :debug/rect]))))

(f/reg-pull ::rect
  (fn [self]
    [:debug/rect self])
  (fn [rect]
    (select-keys rect [:left :top :width :height])))

(def state-h
  (util/derive-pairs
   [[::mounted ::open] ::display]))

(fsm/reg-fsm ::node
  (fn [self]
    {:ref  self
     :attr :debug.panel/state
     :fsm  {nil    {[::open self] ::open}
            ::open {[::close self] nil}}}))

(fsm/reg-fsm ::panel
  (fn [self tap el]
    {:ref  self
     :attr :debug.panel/state
     :fsm  {nil       {[::toggle tap] ::mounted}
            ::mounted {[::props-ready self] {:to ::open :dispatch [::set-rect self tap el]}}
            ::open    {[::toggle tap] ::closed}
            ::closed  {[::toggle tap] ::open}}}))

(f/reg-no-op ::toggle ::open ::close ::props-ready)

(def cluster-opts
  {:attrs      {:id #(find % :debug/id)
                :x  #(get-in % [:debug/rect :left])
                :y  #(get-in % [:debug/rect :top])}
   :min-points 2
   :epsilon    50})

(defn create-mark
  [m]
  (let [ref (find m :debug/id)]
    {:debug/type :debug.type/mark
     :overlay/id (str "mark" (second ref))
     :debug/tap  ref}))

(defn create-props
  [m]
  (let [ref (find m :debug/id)]
    {:debug/type :debug.type/props
     :overlay/id (str "props" (second ref))
     :debug/tap  ref}))

(defn create-group
  [xs]
  (let [c (c/centroid xs cluster-opts)]
    {:debug/type     :debug.type/group
     :overlay/id     (str "group" c)
     :debug/group    (map create-mark xs)
     :debug/centroid c}))

(f/reg-pull ::props
  (fn [self]
    [{:debug/tap
      [:debug/type
       :debug/id
       :debug/line
       :debug/refs]}
     self]))

(f/reg-pull ::tap
  (fn [tap]
    [[:debug/id
      :debug/line]
     tap]))

(f/reg-pull ::overlay-panels
  (fn []
    [{::overlay-panels
      [:cmp/uuid
       :debug/type
       :debug/tap]}]))

(f/reg-sub ::overlay-nodes
  (fn [_]
    (f/sub [::d/taps]))
  (fn [taps _]
    (let [t      (vals taps)
          g      (c/cluster t cluster-opts)
          marks  (map create-mark (:noise g))
          props  (map create-props t)
          groups (map create-group (vals (dissoc g :noise)))]
      (concat marks groups props))))

(f/reg-sub ::overlay
  (fn [_]
    [(f/sub [::overlay-nodes])
     (f/sub [::overlay-panels])])
  (fn [[nodes panels] _]
    (concat nodes panels)))

(f/reg-sub ::render)

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

(f/reg-event-fx ::move
  (fn [{db :db} [_ e-drag ref dx dy w h]]
    (let [{vw :width
           vh :height} (viewport-size)

          x    (.-clientX e-drag)
          y    (.-clientY e-drag)
          l    (-> (max (- x dx) 0)
                   (min (- vw w)))
          t    (-> (max (- y dy) 0)
                   (min (- vh h)))
          rect {:left l :top t}]
      {:db (db/update-inn db [ref :debug/rect] merge rect)})))

(f/reg-event-fx ::resize
  (fn [{db :db} [_ e-drag ref dx dy w h]]
    (letfn [(f [r]
              (let [x (.-clientX e-drag)
                    y (.-clientY e-drag)]
                (-> r
                    (assoc :width (+ (- x (:left r)) (- w dx)))
                    (assoc :height (+ (- y (:top r)) (- h dy))))))]
      {:db (db/update-inn db [ref :debug/rect] f)})))

(defn drag-listener!
  [db handler ref e-mouse-down]
  (let [{l :left
         t :top
         w :width
         h :height} (db/get-inn db [ref :debug/rect])

        x  (.-clientX e-mouse-down)
        y  (.-clientY e-mouse-down)
        dx (- x l)
        dy (- y t)]
    (.preventDefault e-mouse-down)
    (.stopPropagation e-mouse-down)
    (fn [e-drag]
      (.preventDefault e-drag)
      (.stopPropagation e-drag)
      (f/disp-sync [handler e-drag ref dx dy w h]))))

(defn drag-unlistener!
  [listener]
  (fn anon [_]
    (.removeEventListener js/document "mousemove" listener)
    (.removeEventListener js/document "mouseup" anon)
    (f/disp-sync [::drag-stop!])))

(f/reg-event-fx ::drag!
  (fn [{db :db} [_ handler ref e-mouse-down]]
    {:pre [handler]}
    (let [on-f (drag-listener! db handler ref e-mouse-down)
          un-f (drag-unlistener! on-f)]
      (.addEventListener js/document "mousemove" on-f)
      (.addEventListener js/document "mouseup" un-f)
      {:db (assoc db ::dragging true)})))

(f/reg-event-db ::drag-stop!
  (fn [db _]
    (assoc db ::dragging false)))
