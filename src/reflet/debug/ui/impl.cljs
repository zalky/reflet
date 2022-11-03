(ns reflet.debug.ui.impl
  (:require [cinch.core :as util]
            [reflet.core :as f]
            [reflet.db :as db]
            [reflet.debug :as d]
            [reflet.debug.cluster :as c]
            [reflet.fsm :as fsm]
            [reflet.interop :as i]))

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

(defn get-z-index
  [db]
  (get db ::z-index 1000000000))

(defn update-z-index
  "Attempt to start panels ontop all host UI elements. This still
  leaves over a billion panel interactions. Worst case the panels stop
  layering properly."
  [db ref]
  (let [z (get-z-index db)]
    (-> db
        (assoc ::z-index (inc z))
        (db/update-inn [ref :debug/rect] assoc :z-index z))))

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

;;;; Component queries and mutations

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
           t :top}    target-rect
          {w :width
           h :height} (rect el)]
      {:left   (max (- l w) 0)
       :top    (max (- t h) 0)
       :width  w
       :height h})))

(def cmp-hierarchy
  (util/derive-pairs
   [[:debug.type/ref-panel
     :debug.type/props-panel] ::panel]))

(defmulti get-rect
  (fn [db self & _]
    (db/get-inn db [self :debug/type]))
  :hierarchy #'cmp-hierarchy)

(def max-init-panel-height
  500)

(defn pos-or-default
  [pos dim]
  (+ (or pos (/ (get (viewport-size) dim) 4)) 50))

(defmethod get-rect ::panel
  [db _ el]
  (let [source-el   (::selected db)
        {t :top
         l :left}   (some-> source-el i/grab rect)
        {h :height} (some-> el i/grab rect)]
    {:left   (pos-or-default l :height)
     :top    (pos-or-default t :width)
     :width  300
     :height (some-> h (min max-init-panel-height))}))

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

(f/reg-event-db ::set-height
  (fn [db [_ self el]]
    (letfn [(f [r]
              (->> (i/grab el)
                   (panel-content-height)
                   (assoc r :height)))]
      (-> db
          (db/update-inn [self :debug/rect] f)
          (update-z-index self)))))

(f/reg-pull ::rect
  (fn [self]
    [:debug/rect self])
  (fn [rect]
    (select-keys rect [:left :top :width :height :z-index])))

(defn- v-offset
  [lh el]
  (mod (panel-content-height el) lh))

(f/reg-sub ::rect-quantize
  ;; Quantize size and position with respect to line-height. For nicer
  ;; visual results when quantizing element height, we preserve
  ;; whatever height offset the the element originally had from a
  ;; quantization point. The end result is that the element height
  ;; will initially fit the content, and then shrink or expand by the
  ;; quantization factor (line-height). All other dimensions just snap
  ;; directly to quantization points.
  (fn [[_ self el]]
    [(f/sub [::rect self])
     (f/sub [::i/grab el])])
  (fn [[rect ^js el] _]
    (when (and el (not-empty rect))
      (let [qfn    (comp int quant)
            lh     (px el :line-height)
            offset (v-offset lh el)]
        (-> rect
            (update :left qfn lh)
            (update :top qfn lh)
            (update :width qfn lh)
            (update :height #(+ (quant % lh) offset)))))))

(f/reg-event-db ::set-props
  (fn [db [_ self props]]
    (as-> (apply assoc props self) %
      (dissoc % :debug/el :debug/self)
      (db/mergen db %))))

(def state-hierarchy
  (util/derive-pairs
   [[::mounted ::open] ::display]))

(fsm/reg-fsm ::node-fsm
  (fn [& [self :as args]]
    (let [e (vec (cons ::set-rect args))]
      {:ref self
       :fsm {nil       {[::set-props self] ::mounted}
             ::mounted {[::ready-to-size self] {:to ::closed :dispatch e}}
             ::closed  {[::open self] ::open}
             ::open    {[::close self] ::closed}}})))

(fsm/reg-fsm ::panel-fsm
  (fn [self el tap]
    (let [e [::set-rect self el]]
      {:ref self
       :fsm {nil       {[::set-props self] ::mounted}
             ::mounted {[::ready-to-size self] {:to ::open :dispatch e}}
             ::open    {[::close-panel tap] nil}}})))

(f/reg-event-db ::choose-lens
  (fn [db [_ self lens]]
    (db/assoc-inn db [self :debug/lens] lens)))

(f/reg-pull ::lens
  (fn [self]
    [:debug/lens self]))

(f/reg-no-op ::open ::close ::ready-to-size)

(def cluster-opts
  {:attrs      {:id #(find % :debug/id)
                :x  #(get-in % [:debug/rect :left])
                :y  #(get-in % [:debug/rect :top])}
   :min-points 2
   :epsilon    50})

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

(f/reg-pull ::props-panel
  (fn [self]
    [{:debug/tap
      [:debug/id
       :debug/ns
       :debug/line
       :debug/props]}
     self]))

(f/reg-pull ::tap
  (fn [tap]
    [[:debug/id
      :debug/ns
      :debug/line]
     tap]))

(f/reg-event-db ::open-prop
  (fn [db [_ ref]]
    (->> ref
         (create-props-panel)
         (assoc-in db [::panels ref]))))

(f/reg-event-db ::open-ref
  (fn [db [_ ref]]
    (let [p (create-ref-panel ref)]
      (assoc-in db [::panels (:debug/self p)] p))))

(f/reg-event-db ::close-props-panel
  (fn [db [_ tap]]
    (-> db
        (update ::panels dissoc tap)
        (dissoc ::selected))))

(f/reg-event-db ::close-ref-panel
  (fn [db [_ self]]
    (-> db
        (update ::panels dissoc self)
        (dissoc ::selected)
        (db/dissocn self))))

(f/reg-sub ::overlay-panels
  (fn [db _]
    (vals (get db ::panels))))

(f/reg-sub ::overlay-nodes
  (fn [_]
    (f/sub [::d/taps]))
  (fn [taps _]
    (let [t      (vals taps)
          g      (c/cluster t cluster-opts)
          marks  (map create-mark (:noise g))
          groups (map create-mark-group (vals (dissoc g :noise)))]
      (concat marks groups))))

(f/reg-sub ::overlay
  (fn [_]
    [(f/sub [::overlay-nodes])
     (f/sub [::overlay-panels])])
  (fn [[nodes panels] _]
    (concat nodes panels)))

(f/reg-sub ::render)
