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
  [db p g]
  (assoc-in db [:group p] g))

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

(defn neigh
  [db id]
  (get-in db [:neighbours id]))

(defn index-neigh
  [{points         :points
    n              :neighbours
    {eps :epsilon} :opts
    :as            db} q]
  (letfn [(f [n p]
            (let [q* (get points q)
                  p* (get points p)
                  d  (euclidean-distance q* p*)]
              (if (<= d eps)
                (update n q conj p)
                n)))]
    (if-not (neigh db q)
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
  [db seed g]
  (loop [db         db
         done       #{}
         [q & more] seed]
    (if q
      (let [done* (conj done q)
            g*    (get-group db q)]
        (if (= g* :noise)
          (recur (set-group db q g) done* more)
          (if g*
            (recur db done* more)
            (let [db     (set-group db q g)
                  db     (index-neigh db q)
                  minp   (:min-points (:opts db))
                  grow-n (neigh db q)]
              (if (<= minp (count grow-n))
                (->> done*
                     (grow more grow-n)
                     (recur db done*))
                (recur db done* more))))))
      db)))

(defn dbscan
  "Simple DBSCAN implementation. See
  https://en.wikipedia.org/wiki/DBSCAN

  This may not be the most ideal clustering algorithm for UI
  applications, since it can produce clusters that are arbitrarily
  large as long as they are densly connected. However, given that UI
  elements also possess a high degree of geometric regularity, with a
  correctly tuned `:epsilon` parameter it should still be able to
  produce good results in almost any context."
  [db]
  (loop [db         db
         g          0
         [p & more] (points db)]
    (if p
      (if (get-group db p)
        (recur db g more)
        (let [db   (index-neigh db p)
              n    (neigh db p)
              minp (:min-points (:opts db))]
          (if (< (count n) minp)
            (-> (set-group db p :noise)
                (recur g more))
            (let [g    (inc g)
                  db   (set-group db p g)
                  seed (disj n p)]
              (-> (grow-neigh db seed g)
                  (recur g more))))))
      db)))

(defn grid
  "Dead simple grid clustering."
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
               (assoc db (id-f p) %)))
           {}
           points)})

(defn cluster
  [points {:keys [algo]
           :or   {algo dbscan}
           :as   opts}]
  (let [db (create-db points opts)
        r  (algo db)]
    (group-by (comp (:group r) :id) points)))

