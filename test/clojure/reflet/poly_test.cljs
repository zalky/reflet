(ns reflet.poly-test
  (:require [cinch.core :as util]
            [cljs.test :as t :refer [is]]
            [reflet.fixtures :as fix]
            [reflet.poly :as p]))

(t/use-fixtures :each fix/base-fixtures)

(def entries
  {::a "a"
   ::b "b"})

(t/deftest poly-resolve-test
  (is (= (p/poly-resolve
          {:entries      {nil   "nil"
                          false "false"}
           :dispatch-val nil})
         "nil"))
  (is (= (p/poly-resolve
          {:entries      {nil   "nil"
                          false "false"}
           :dispatch-val false})
         "false"))
  (is (= (p/poly-resolve
          {:entries      entries
           :dispatch-val ::a})
         "a"))
  (is (= (p/poly-resolve
          {:entries      (assoc entries :default "default")
           :dispatch-val ::missing})
         "default"))
  (is (= (p/poly-resolve
          {:entries              (assoc entries :custom-default "custom-default")
           :dispatch-val         ::missing
           :default-dispatch-val :custom-default})
         "custom-default"))
  (is (= (p/poly-resolve
          {:entries      entries
           :hierarchy    (util/derive-pairs [::a ::b])
           :dispatch-val ::a})
         "a"))
  (is (= (p/poly-resolve
          {:entries      entries
           :hierarchy    (util/derive-pairs [::d ::a])
           :dispatch-val ::d})
         "a"))
  (is (= (p/poly-resolve
          {:entries      entries
           :hierarchy    (util/derive-pairs [::d ::a ::e ::d ::f ::e])
           :dispatch-val ::f})
         "a"))
  (is (= (p/poly-resolve
          {:entries      entries
           :hierarchy    (util/derive-pairs [::d [::a ::b]])
           :prefers      {::a #{::b}}
           :dispatch-val ::d})
         "a"))
  (is (= (p/poly-resolve
          {:entries      {[::a* ::b] "one"
                          [::a ::b*] "two"}
           :hierarchy    (util/derive-pairs [::a ::a* ::b ::b*])
           :prefers      {[::a* ::b] #{[::a ::b*]}}
           :dispatch-val [::a ::b]})
         "one")))

(t/deftest poly-resolve-error-test
  (is (thrown-with-msg? js/Error
                        #"Could not resolve entry"
                        (p/poly-resolve nil)))
  (is (thrown-with-msg? js/Error
                        #"Could not resolve entry"
                        (p/poly-resolve
                         {:entries      entries
                          :dispatch-val ::missing})))
  (is (thrown-with-msg? js/Error
                        #"Multiple dispatch entries"
                        (p/poly-resolve
                         {:entries      entries
                          :hierarchy    (util/derive-pairs [::d [::a ::b]])
                          :dispatch-val ::d})))
  (is (thrown-with-msg? js/Error
                        #"Multiple dispatch entries"
                        (p/poly-resolve
                         {:entries      {[::a* ::b] "one"
                                         [::a ::b*] "two"}
                          :hierarchy    (util/derive-pairs [::a ::a* ::b ::b*])
                          :dispatch-val [::a ::b]}))))

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
