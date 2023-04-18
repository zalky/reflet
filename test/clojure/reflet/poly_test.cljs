(ns reflet.poly-test
  (:require [cinch.core :as util]
            [cljs.test :as t :refer [is]]
            [reflet.fixtures :as fix]
            [reflet.poly :as p]))

(t/use-fixtures :each fix/base-fixtures)

(def hierarchy
  (util/derive-pairs
   [::a ::b
    ::a ::c
    ::c ::d
    ::b ::e]))

(t/deftest prefers?-test
  (is (false? (p/prefers? {} ::a ::b)))
  (is (false? (p/prefers? {} ::a ::b)))
  (is (false? (p/prefers? {::b #{::a}} ::a ::b)))
  (is (false? (p/prefers? {::a #{::c}} ::e ::c)))
  (is (false? (p/prefers? {::e #{::d}} ::c ::b)))
  (is (false? (p/prefers? {::b #{::a} ::c #{::b}} ::a ::b)))
  (is (false? (p/prefers? {::b #{::a} ::c #{::b}} [::a ::c] [::b ::b])))

  (is (true? (p/prefers? {::a #{::b}} ::a ::b)))
  (is (true? (p/prefers? {::a #{::c}} ::a ::c)))
  (is (true? (p/prefers? {[::a ::b] #{[::b ::c]}} [::a ::b] [::b ::c]))))

(t/deftest prefer-pairs-test
  (is (= (p/prefer-pairs [])
         {}))
  (is (= (p/prefer-pairs [])
         {}))
  (is (= (p/prefer-pairs [::a ::b])
         {::a #{::b}}))
  (is (= (p/prefer-pairs [::a [::b ::c]])
         {::a #{[::b ::c]}}))
  (is (= (p/prefer-pairs [[::a ::b] ::c])
         {[::a ::b] #{::c}}))
  (is (= (p/prefer-pairs [[::a ::b] [::c ::d]])
         {[::a ::b] #{[::c ::d]}}))
  (is (= (p/prefer-pairs {::a #{::b}} [::a ::b])
         {::a #{::b}}))
  (is (= (p/prefer-pairs {::a #{::c}} [::a ::b])
         {::a #{::b ::c}}))
  (is (thrown-with-msg? js/Error
                        #"is already preferred"
                        (p/prefer-pairs {::a #{::c}} [::c ::a]))))
