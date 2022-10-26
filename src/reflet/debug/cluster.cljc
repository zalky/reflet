(ns reflet.debug.cluster
  "Simple DBSCAN implementation.

  See https://en.wikipedia.org/wiki/DBSCAN"
  (:require [clojure.set :as set]))

(defn points
  [db]
  (keys (:points db)))

(defn get-group
  [db p]
  (get-in db [:group p]))

(defn set-group
  [db p group]
  (assoc-in db [:group p] group))

(defn pow
  [x y]
  #?(:cljs (js/Math.pow x y)
     :clj  (Math/pow x y)))

(defn sqrt
  [x]
  #?(:cljs (js/Math.sqrt x)
     :clj  (Math/sqrt x)))

(defn euclidean-distance
  [{x1 :x y1 :y}
   {x2 :x y2 :y}]
  (sqrt (+ (pow (- x2 x1) 2)
           (pow (- y2 y1) 2))))

(defn get-neigh
  [db id]
  (get-in db [:neighbours id]))

(defn index-neigh
  [{points      :points
    n           :neighbours
    {eps  :epsilon
     dist :distance-fn
     :or  {dist euclidean-distance}
     :as  opts} :opts
    :as         db} q]
  (letfn [(f [n p]
            (let [q* (get points q)
                  p* (get points p)
                  d  (dist q* p*)]
              (if (<= d eps)
                (update n q conj p)
                n)))]
    (if-not (get-neigh db q)
      (->> (keys points)
           (reduce f (assoc n q #{}))
           (assoc db :neighbours))
      db)))

(defn grow
  [more grow-n done]
  (->> done
       (set/difference grow-n)
       (concat more)
       (distinct)))

(defn grow-neigh
  [{{:keys [min-points]} :opts
    :as                  db} seed group]
  (loop [db         db
         done       #{}
         [q & more] seed]
    (if q
      (let [done*  (conj done q)
            group* (get-group db q)]
        (if (= group* :noise)
          (recur (set-group db q group) done* more)
          (if group*
            (recur db done* more)
            (let [db     (set-group db q group)
                  db     (index-neigh db q)
                  grow-n (get-neigh db q)]
              (if (<= min-points (count grow-n))
                (recur db done* (grow more grow-n done*))
                (recur db done* more))))))
      db)))

(defn dbscan
  "Simple DBSCAN implementation. See
  https://en.wikipedia.org/wiki/DBSCAN

  One potential drawback of this algorithm for this application is
  that it can produce clusters that are arbitrarily large as long as
  they are densly connected. However, UI elements possess a high
  degree of geometric regularity, and so with a correctly tuned
  `:epsilon` parameter it should be able to produce good results. The
  naive grid clustering approach below, while guaranteeing bounded
  clusters, can potentially fail to cluster points that are adjacent,
  but on opposite sides of a partition. There are hierarchical grid
  clustering algorithms that might mitigate this issue, but with the
  right tuning DBSCAN should be fine."
  [{{:keys [min-points]} :opts
    :as                  db}]
  (loop [db         db
         group      0
         [p & more] (points db)]
    (if p
      (if (get-group db p)
        (recur db group more)
        (let [db (index-neigh db p)
              n  (get-neigh db p)]
          (if (< (count n) min-points)
            (recur (set-group db p :noise) group more)
            (let [group (inc group)
                  seed  (disj n p)]
              (-> db
                  (set-group p group)
                  (grow-neigh seed group)
                  (recur group more))))))
      db)))

(defn grid
  "Simple grid clustering."
  [{{[quant-x
      quant-y] :quant} :opts
    :as                db}]
  {:pre [quant-x quant-y]}
  (reduce
   (fn [db p]
     (let [x (get-in db [:points p :x])
           y (get-in db [:points p :y])
           g [(quot x quant-x)
              (quot y quant-y)]]
       (set-group db p g)))
   db
   (points db)))

(defn create-db
  [points {{id-f :id
            x-f  :x
            y-f  :y
            :or  {id-f :id
                  x-f  :x
                  y-f  :y}} :attrs
           :as              opts}]
  {:opts opts
   :points
   (reduce (fn [db p]
             (as-> {} %
               (assoc % :x (x-f p))
               (assoc % :y (y-f p))
               (assoc % :id (id-f p))
               (assoc db (id-f p) %)))
           {}
           points)})

(defn cluster
  [points {{id-f :id
            :or  {id-f :id}} :attrs

           :keys [algo]
           :or   {algo dbscan}
           :as   opts}]
  (let [db (create-db points opts)
        r  (algo db)]
    (group-by (comp (:group r) id-f) points)))

(defn centroid
  [group {{x-f :x
           y-f :y
           :or {x-f :x
                y-f :y}} :attrs}]
  (letfn [(u [f]
            (-> (transduce (map f) + group)
                (/ (count group))
                (float)))]
    {:x (u x-f)
     :y (u y-f)}))
