(ns reflet.util-test
  (:require [clojure.test :refer [deftest testing is]]
            [reflet.util :as util]))

(deftest test-derive-pairs
  (testing "Derive pairs into heirarchy"
    (is (= (util/derive-pairs nil)
           {:parents {}, :descendants {}, :ancestors {}}))
    (is (= (util/derive-pairs [])
           {:parents {}, :descendants {}, :ancestors {}}))
    (is (= (util/derive-pairs [:a :b])
           (-> (make-hierarchy)
               (derive :a :b))))
    (is (= (util/derive-pairs [:a :b :b :c])
           (-> (make-hierarchy)
               (derive :a :b)
               (derive :b :c))))
    (is (= (util/derive-pairs [[:a :b] :c])
           (-> (make-hierarchy)
               (derive :a :c)
               (derive :b :c))))))

(deftest test-merge-hierarchies
  (testing "Hierarchy merge basics"
    (is (= (util/merge-hierarchies nil nil)
           nil))
    (is (= (util/merge-hierarchies (make-hierarchy) nil)
           {:parents {}, :descendants {}, :ancestors {}}))
    (is (= (util/merge-hierarchies (make-hierarchy) (make-hierarchy))
           {:parents {}, :descendants {}, :ancestors {}})))

  (testing "Hierarchy merge derives"
    (is (= (util/merge-hierarchies (-> (make-hierarchy) (derive :a :b))
                                   (-> (make-hierarchy) (derive :c :d)))
           (-> (make-hierarchy) (derive :a :b) (derive :c :d))))
    (is (= (util/merge-hierarchies (-> (make-hierarchy) (derive :a :b))
                                   (-> (make-hierarchy) (derive :b :c)))
           (-> (make-hierarchy) (derive :a :b) (derive :b :c))))
    (is (= (util/merge-hierarchies (-> (make-hierarchy) (derive :a :b))
                                   (-> (make-hierarchy) (derive :c :b)))
           (-> (make-hierarchy) (derive :a :b) (derive :c :b))))
    (is (= (util/merge-hierarchies (-> (make-hierarchy) (derive :a :b))
                                   (-> (make-hierarchy) (derive :a :c)))
           (-> (make-hierarchy) (derive :a :b) (derive :a :c)))))

  (let [x (-> (make-hierarchy) (derive :a :b))
        y (-> (make-hierarchy) (derive :b :c))
        z (-> (make-hierarchy) (derive :a :c))]
    (testing "Hierarchy merge commutative property"
      (is (= (util/merge-hierarchies x y)
             (util/merge-hierarchies y x))))

    (testing "Hierarchy merge associative property"
      (is (= (util/merge-hierarchies x (util/merge-hierarchies y z))
             (util/merge-hierarchies (util/merge-hierarchies x y) z)))))

  (testing "Cyclical derivations"
    (is (thrown? #?(:clj Exception :cljs js/Error)
                 (util/merge-hierarchies
                  (-> (make-hierarchy) (derive :a :b))
                  (-> (make-hierarchy) (derive :b :a)))))
    (is (thrown? #?(:clj Exception :cljs js/Error)
                 (util/merge-hierarchies
                  (-> (make-hierarchy) (derive :a :b) (derive :b :c))
                  (-> (make-hierarchy) (derive :c :a)))))))

