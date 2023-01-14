(ns reflet.debug.ui.impl
  (:require [cinch.core :as util]
            [clojure.set :as set]
            [reagent.ratom :as r]
            [reflet.config :as config]
            [reflet.core :as f]
            [reflet.db :as db]
            [reflet.debug :as d]
            [reflet.debug.cluster :as c]
            [reflet.fsm :as fsm]
            [reflet.interop :as i]))

(def cmp-hierarchy
  (util/derive-pairs
   [[:debug.type/mark
     :debug.type/mark-group] ::mark
    [:debug.type/ref-panel
     :debug.type/props-panel] ::panel]))

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

(defn computed-style
  [el k]
  (-> el
      (js/getComputedStyle)
      (.getPropertyValue (name k))))

(defn px
  "Gets the float value of the computed style."
  [el k]
  (js/parseFloat (computed-style el k)))

(defn quant
  [n m]
  (* (quot n m) m))

(f/reg-sub ::dragging
  (fn [db _]
    (get db ::dragging)))

(f/reg-sub ::selected
  (fn [db _]
    (get db ::selected)))

(f/reg-event-db ::select
  (fn [db [_ ref]]
    (assoc db ::selected ref)))

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

(def z-index-0
  1000000000)

(defn get-z-index
  [db]
  (get db ::z-index z-index-0))

(defmulti update-z-index
  "Attempt to start panels on top all host UI elements. This still
  leaves over a billion panel interactions. Worst case the panels stop
  layering properly."
  (fn [db ref]
    (db/get-inn db [ref :debug/type]))
  :hierarchy #'cmp-hierarchy)

(defmethod update-z-index :default
  [db ref]
  (let [z (get-z-index db)]
    (-> db
        (assoc ::z-index (inc z))
        (db/update-inn [ref :debug/rect] assoc :z-index z))))

(defmethod update-z-index ::mark
  [db ref]
  (db/update-inn db [ref :debug/rect] assoc :z-index z-index-0))

(f/reg-event-fx ::drag!
  (fn [{db :db} [_ handler ref e-mouse-down]]
    {:pre [handler]}
    (let [on-f (drag-listener! db handler ref e-mouse-down)
          un-f (drag-unlistener! on-f)]
      (.addEventListener js/document "mousemove" on-f)
      (.addEventListener js/document "mouseup" un-f)
      {:db (-> db
               (assoc ::dragging handler)
               (assoc ::selected ref)
               (update-z-index ref))})))

(f/reg-event-db ::drag-stop!
  (fn [db _]
    (dissoc db ::dragging)))

;;;; Events and queries

(defn rect
  "Keep in mind that .getBoundingClientRect will return zeros during
  certain phases of the react lifecycle. Use with care."
  [el & [selectors]]
  (let [r (.getBoundingClientRect el)]
    {:top    (.-top r)
     :bottom (.-bottom r)
     :left   (.-left r)
     :right  (.-right r)
     :width  (.-width r)
     :height (.-height r)}))

(defn- shift-rect
  [el target-rect]
  (when el
    (let [{l :left
           t :top
           z :z-index} target-rect
          {w :width
           h :height}  (rect el)]
      {:left    (max (- l w) 0)
       :top     (max (- t h) 0)
       :width   w
       :height  h
       :z-index z})))

(defmulti get-rect
  (fn [db self & _]
    (db/get-inn db [self :debug/type]))
  :hierarchy #'cmp-hierarchy)

(def new-panel-offset
  50)

(def new-panel-width
  315)

(defn- selected-el
  [db]
  (db/get-inn db [(::selected db) :debug/el]))

(defmethod get-rect ::panel
  [db _ target-el]
  (let [sel-el       (selected-el db)
        {st :top
         sl :left}   (some-> sel-el i/grab rect)
        {th :height} (some-> target-el i/grab rect)
        {vh :height
         vw :width}  (viewport-size)]
    {:width new-panel-width
     :left  (min (- vw new-panel-width)
                 (+ sl new-panel-offset))
     :top   (min (- vh th)
                 (+ st new-panel-offset))}))

(defmethod get-rect :debug.type/mark-group
  [_ _ el {:keys [x y]}]
  (->> {:left x :top y}
       (shift-rect (i/grab el))))

(defmethod get-rect :debug.type/mark
  [db _ el tap]
  (->> (db/get-inn db [tap :debug/rect])
       (shift-rect (i/grab el))))

(defn- panel-content-height
  [el]
  (let [b (* 2 (px el :border-width))]
    (->> (.. el -firstChild -children)
         (array-seq)
         (map #(px % :height))
         (reduce + b))))

(f/reg-event-db ::set-rect
  (fn [db [_ self & args]]
    (letfn [(f [r]
              (or r (apply get-rect db self args)))]
      (-> db
          (db/update-inn [self :debug/rect] f)
          (update-z-index self)))))

(def max-init-panel-height
  275)

(f/reg-event-db ::set-height
  (fn [db [_ self el]]
    (letfn [(f [r]
              (->> (i/grab el)
                   (panel-content-height)
                   (min max-init-panel-height)
                   (assoc r :height)))]
      (db/update-inn db [self :debug/rect] f))))

(f/reg-pull ::rect
  (fn [self]
    [:debug/rect self])
  (fn [rect]
    (select-keys rect [:left :top :width :height :z-index])))

(f/reg-pull ::mark-rect*
  (fn [self]
    [[:debug/rect
      {:debug/tap [:debug/rect]}]
     self])
  (fn [{{z :z-index}    :debug/rect
        {r :debug/rect} :debug/tap}]
    (-> r
        (assoc :z-index z)
        (select-keys [:left :top :width :height :z-index]))))

(f/reg-sub ::mark-rect
  (fn [[_ self el]]
    [(f/sub [::mark-rect* self])
     (f/sub [::i/grab el])])
  (fn [[rect el]]
    (shift-rect el rect)))

(defn- get-content-el
  "Returns either the first child if it exists, or the parent content
  element."
  [el]
  (or (aget (.. el -firstChild -children) 1)
      (.. el -firstChild)))

(defn- adjusted-quant-factor
  "This adjusts the vertical quantization factor so that the bottom of
  the panel aligns with the horizontal dividers in FSM, query and
  event panels. It will be fractionally off for the data panels, but
  because they don't have dividers, it will not be perceptible, even
  when zoomed in."
  [el lh]
  (-> (get-content-el el)
      (px :padding-top)
      (* 2)
      (+ 1)                             ; for border width
      (+ lh)
      (/  2)))

(defn- height-qfn
  [height el lh]
  (let [h      (adjusted-quant-factor el lh)
        offset (mod (panel-content-height el) h)]
    (+ (quant height h) offset)))

(f/reg-sub ::rect-quantized
  ;; Quantize size and position with respect to line-height. For nicer
  ;; visual results when quantizing element height, use a quantization
  ;; algorithm that dynamically accounts for content borders and
  ;; padding.
  (fn [[_ self el]]
    [(f/sub [::rect self])
     (f/sub [::i/grab el])])
  (fn [[rect ^js el] _]
    (when (and el (not-empty rect))
      (let [qfn (comp int quant)
            lh  (px el :line-height)]
        (-> rect
            (update :left qfn lh)
            (update :top qfn lh)
            (update :width qfn lh)
            (update :height height-qfn el lh))))))

(f/reg-event-db ::set-props
  (fn [db [_ self props]]
    (as-> (apply assoc props self) %
      (dissoc % :debug/self)
      (db/mergen db %))))

(def state-hierarchy
  (util/derive-pairs
   [[::mounted ::open] ::display]))

(fsm/reg-fsm ::node-fsm
  (fn [self]
    {:ref self
     :fsm {nil      {[::set-props self] ::open}
           ::open   {[::close self] ::closed}
           ::closed {[::open self] ::open}}}))

(fsm/reg-fsm ::panel-fsm
  (fn [self tap]
    {:ref self
     :fsm {nil    {[::set-props self] ::open}
           ::open {[::close-panel tap] nil}}}))

(f/reg-no-op ::open ::close ::ready-to-size)

(def observer-config
  #js {:attributes true
       :childList  true
       :subtree    true})

(f/reg-sub-raw ::observe
  (fn [_ [_ el]]
    (let [tick     (r/atom 0)
          observer (js/MutationObserver. #(swap! tick inc))]
      (.observe observer el observer-config)
      (r/make-reaction
        (fn [] @tick)
        :on-dispose
        (fn [] (.disconnect observer))))))

(f/reg-pull ::lens
  (fn [self]
    [:debug/lens self]))

(f/reg-pull ::trace-n
  (fn [self]
    [:debug/trace-n self])
  (fn [n]
    (or n 0)))

(f/reg-event-db ::set-lens
  (fn [db [_ self lens]]
    (db/assoc-inn db [self :debug/lens] lens)))

(f/reg-event-db ::clear-lens
  (fn [db [_ self]]
    (db/update-inn db [self] dissoc :debug/lens)))

(f/reg-event-db ::inc-trace-n
  (fn [db [_ self max-n]]
    (db/update-inn db [self :debug/trace-n] #(min (inc %) max-n))))

(f/reg-event-db ::dec-trace-n
  (fn [db [_ self]]
    (db/update-inn db [self :debug/trace-n] #(max (dec %) 0))))

(def cluster-opts
  {:attrs      {:id #(find % :debug/id)
                :x  #(get-in % [:debug/rect :left])
                :y  #(get-in % [:debug/rect :top])}
   :min-points 2
   :epsilon    30})

(defn create-mark
  [m]
  (let [[a v :as ref] (find m :debug/id)]
    {:debug/type :debug.type/mark
     :debug/self [:debug/id (str "mark" v)]
     :debug/tap  ref}))

(defn create-mark-group
  [xs]
  (let [c (c/centroid xs cluster-opts)]
    {:debug/type     :debug.type/mark-group
     :debug/self     [:debug/id (str "mark-group" c)]
     :debug/group    (map create-mark xs)
     :debug/centroid c}))

(defn create-ref-panel
  [[a v :as ref]]
  {:debug/type :debug.type/ref-panel
   :debug/self (db/random-ref :debug/id)
   :debug/ref  ref})

(defn create-props-panel
  [[a v :as ref]]
  {:debug/type :debug.type/props-panel
   :debug/self [:debug/id (str "props-panel" v)]
   :debug/tap  ref})

(defn create-context
  [value {:keys [x y]} z]
  {:debug/type  :debug.type.context/ref
   :debug/self  (db/random-ref :debug/id)
   :debug/value value
   :debug/pos   {:left    x
                 :top     y
                 :z-index z}})

(defn create-global-controls
  []
  {:debug/type :debug.type/global-controls
   :debug/self [:debug/id "global-controls"]})

(f/reg-event-fx ::open-context
  (fn [{db :db} [_ value pos]]
    (letfn [(f [e]
              (.removeEventListener js/document "click" f)
              (f/disp [::close-context]))]
      (.addEventListener js/document "click" f)
      (let [z (get-z-index db)]
        {:db (-> (->> z
                      (create-context value pos)
                      (assoc db ::context))
                 (assoc ::z-index (inc z)))}))))

(f/reg-event-db ::close-context
  (fn [db _]
    (dissoc db ::context)))

(f/reg-event-db ::open-prop-panel
  (fn [db [_ ref]]
    (->> ref
         (create-props-panel)
         (assoc-in db [::panels ref]))))

(f/reg-event-db ::open-ref-panel
  (fn [db [_ ref]]
    (let [p (create-ref-panel ref)]
      (assoc-in db [::panels (:debug/self p)] p))))

(f/reg-event-db ::close-props-panel
  ;; Do not clean up after props panel. Props panels retain state,
  ;; such as position.
  (fn [db [_ tap]]
    (-> db
        (update ::panels dissoc tap)
        (dissoc ::selected))))

(f/reg-event-fx ::close-ref-panel
  (fn [{db :db} [_ self]]
    {:db       (-> db
                   (update ::panels dissoc self)
                   (dissoc ::selected))
     :dispatch [::f/cleanup self]}))

(defn- get-ref-panels
  [db]
  (->> (::panels db)
       (keys)
       (filter (comp uuid? second))))

(f/reg-event-fx ::close-all-panels
  (fn [{db :db} _]
    (let [refs (get-ref-panels db)]
      {:db         (dissoc db ::panels ::selected)
       :dispatch-n (for [r refs] [::f/cleanup r])})))

(f/reg-pull ::tap
  (fn [tap]
    [[:debug/id
      :debug/name
      :debug/ns
      :debug/line]
     tap]))

(f/reg-pull ::props-panel
  (fn [self]
    [{:debug/tap
      [:debug/id
       :debug/name
       :debug/ns
       :debug/line
       :debug/props]}
     self]))

(f/reg-sub ::overlay-panels
  (fn [db _]
    (vals (get db ::panels))))

(f/reg-pull ::taps
  (fn []
    [{::d/taps '[*]}]))

(f/reg-sub ::overlay-nodes
  (fn [_]
    (f/sub [::taps]))
  (fn [taps _]
    (let [g      (c/cluster taps cluster-opts)
          marks  (map create-mark (:noise g))
          groups (map create-mark-group (vals (dissoc g :noise)))
          ctrls  (create-global-controls)]
      (concat marks groups [ctrls]))))

(f/reg-sub ::overlay-context
  (fn [db _]
    (some-> db
            (get ::context)
            (vector))))

(f/reg-sub ::overlay
  (fn [_]
    [(f/sub [::overlay-nodes])
     (f/sub [::overlay-panels])
     (f/sub [::overlay-context])])
  (fn [[nodes panels context] _]
    (concat nodes panels context)))

(fsm/reg-fsm ::toggle-marks-fsm
  (fn []
    {:ref  [:debug/id ::toggle]
     :attr :overlay.toggle/marks
     :fsm  {nil  {[::toggle-marks] ::on}
            ::on {[::toggle-marks] nil}}}))

(f/reg-no-op ::toggle-marks)

(defn- hotkey?
  [^js e c]
  (and (= (.-key e) c)
       (.-ctrlKey e)
       (not (.-repeat e))))

(defn- get-hotkey
  []
  (-> (config/get-config)
      (get :debug-hotkey \j)))

(defn- overlay-toggle
  [^js e]
  (let [c (get-hotkey)]
    (when (hotkey? e c)
      (.preventDefault e)
      (f/disp [::toggle-marks]))))

(f/reg-event-db ::config
  (fn [db _]
    (if-not (::configured db)
      (do (.addEventListener js/window "keydown" overlay-toggle)
          (-> db
              (assoc ::configured true)
              (update ::db/id-attrs set/union db/default-unique-attributes)))
      db)))
