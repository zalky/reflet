(ns reflet.debug.cluster-test
  (:require [clojure.set :as set]
            [clojure.test :refer [deftest testing is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [reflet.debug.cluster :as c]))

(def test-points
  [{:id 1 :x 1 :y 0}
   {:id 2 :x 1 :y 0}
   {:id 3 :x 1 :y 2}
   {:id 4 :x 2 :y 2}
   {:id 5 :x 5 :y 7}
   {:id 6 :x 6 :y 7}
   {:id 7 :x 30 :y 40}])

(deftest dbscan-test
  (testing "nil"
    (is (nil? (c/dbscan nil)))
    (is (= (c/dbscan {})
           {}))
    (is (= (c/dbscan {:points {}})
           {:points {}})))

  (testing "points"
    (is (= (c/dbscan {:points {1 {:id 1 :x 1 :y 0}}
                      :opts   {:min-points 2 :epsilon 2}})
           {:points     {1 {:id 1 :x 1 :y 0}}
            :neighbours {1 #{1}}
            :group      {1 :noise}
            :opts       {:min-points 2 :epsilon 2}}))
    (is (= (c/dbscan {:points {1 {:id 1 :x 1 :y 0}
                               2 {:id 2 :x 1 :y 0}}
                      :opts   {:min-points 2 :epsilon 2}})
           {:points     {1 {:id 1 :x 1 :y 0}
                         2 {:id 2 :x 1 :y 0}}
            :neighbours {1 #{1 2}
                         2 #{1 2}}
            :group      {1 1
                         2 1}
            :opts       {:min-points 2 :epsilon 2}}))
    (is (= (c/dbscan {:points {1 {:id 1 :x 1 :y 0}
                               2 {:id 2 :x 1 :y 0}
                               3 {:id 3 :x 0 :y 1}}
                      :opts   {:min-points 2 :epsilon 2}})
           {:points
            {1 {:id 1 :x 1 :y 0}
             2 {:id 2 :x 1 :y 0}
             3 {:id 3 :x 0 :y 1}}
            :neighbours {1 #{1 3 2}
                         3 #{1 3 2}
                         2 #{1 3 2}}
            :group      {1 1
                         3 1
                         2 1}
            :opts       {:min-points 2 :epsilon 2}}))
    (testing "changed epsilon"
      (is (= (c/dbscan {:points {1 {:id 1 :x 1 :y 0}
                                 2 {:id 2 :x 1 :y 0}
                                 3 {:id 3 :x 0 :y 1}}
                        :opts   {:min-points 2 :epsilon 1}})
             {:points
              {1 {:id 1 :x 1 :y 0}
               2 {:id 2 :x 1 :y 0}
               3 {:id 3 :x 0 :y 1}}
              :neighbours {1 #{1 2}
                           2 #{1 2}
                           3 #{3}}
              :group      {1 1
                           2 1
                           3 :noise}
              :opts       {:min-points 2 :epsilon 1}})))
    (is (= (c/dbscan {:points {1 {:id 1 :x 1 :y 0}
                               2 {:id 2 :x 1 :y 2}
                               3 {:id 3 :x 6 :y 4}
                               4 {:id 3 :x 6 :y 5}}
                      :opts   {:min-points 2 :epsilon 4}})
           {:points
            {1 {:id 1 :x 1 :y 0}
             2 {:id 2 :x 1 :y 2}
             3 {:id 3 :x 6 :y 4}
             4 {:id 3 :x 6 :y 5}}
            :neighbours {1 #{1 2}
                         2 #{1 2}
                         3 #{4 3}
                         4 #{4 3}}
            :group      {1 1
                         2 1
                         3 2
                         4 2}
            :opts       {:min-points 2 :epsilon 4}}))
    (is (= (-> test-points
               (c/create-db {:min-points 2 :epsilon 2})
               (c/dbscan))
           {:points     {1 {:id 1 :x 1 :y 0}
                         2 {:id 2 :x 1 :y 0}
                         3 {:id 3 :x 1 :y 2}
                         4 {:id 4 :x 2 :y 2}
                         5 {:id 5 :x 5 :y 7}
                         6 {:id 6 :x 6 :y 7}
                         7 {:id 7 :x 30 :y 40}}
            :neighbours {1 #{1 3 2}
                         3 #{1 4 3 2}
                         2 #{1 3 2}
                         4 #{4 3}
                         5 #{6 5}
                         6 #{6 5}
                         7 #{7}}
            :group      {1 1
                         3 1
                         2 1
                         4 1
                         5 2
                         6 2
                         7 :noise}
            :opts       {:min-points 2 :epsilon 2}}))))


(deftest cluster-test
  (testing "clustering API"
    (testing "dbscan"
      (is (= (c/cluster test-points {:min-points 2 :epsilon 2})
             {1      [{:id 1 :x 1 :y 0}
                      {:id 2 :x 1 :y 0}
                      {:id 3 :x 1 :y 2}
                      {:id 4 :x 2 :y 2}]
              2      [{:id 5 :x 5 :y 7}
                      {:id 6 :x 6 :y 7}]
              :noise [{:id 7 :x 30 :y 40}]})))

    (testing "grid"
      (is (= (c/cluster test-points {:algo c/grid :quant [3 3]})
             {[0 0]   [{:id 1 :x 1 :y 0}
                       {:id 2 :x 1 :y 0}
                       {:id 3 :x 1 :y 2}
                       {:id 4 :x 2 :y 2}]
              [1 2]   [{:id 5 :x 5 :y 7}]
              [2 2]   [{:id 6 :x 6 :y 7}]
              [10 13] [{:id 7 :x 30 :y 40}]}))

      (is (= (c/cluster test-points {:algo c/grid :quant [4 4]})
             {[0 0]  [{:id 1 :x 1 :y 0}
                      {:id 2 :x 1 :y 0}
                      {:id 3 :x 1 :y 2}
                      {:id 4 :x 2 :y 2}]
              [1 1]  [{:id 5 :x 5 :y 7}
                      {:id 6 :x 6 :y 7}]
              [7 10] [{:id 7 :x 30 :y 40}]})))))

(defn points-gen
  []
  (->> (gen/let [x  (gen/choose -100 100)
                 y  (gen/choose -100 100)
                 id (gen/choose -100 100)]
         {:id id :x x :y y})
       (gen/vector-distinct-by :id)))

(defn dense-or-densly-reachable?
  [p n {minp :min-points}]
  (or (<= minp (count (n p)))
      (some #(<= minp (count (n %))) (n p))))

(defspec dense-or-densly-reachable-test #?(:clj 1000 :cljs 100)
  ;; Every point in every cluster (not including :noise points) should
  ;; be either dense or densely reachable. See the description of the
  ;; DBSCAN algorithm for what this means:
  ;; https://en.wikipedia.org/wiki/DBSCAN
  (prop/for-all [xs (points-gen)]
    (let [opts {:min-points 2 :epsilon 3}
          db   (c/create-db xs opts)

          {g :group
           n :neighbours} (c/dbscan db)]
      (->> (c/points db)
           (group-by g)
           (#(dissoc % :noise))
           (mapcat second)
           (every? #(dense-or-densly-reachable? % n opts))))))
