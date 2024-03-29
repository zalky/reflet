(ns reflet.client.ui.workflow.impl
  (:require [reflet.core :as f]
            [reflet.db :as db]
            [reflet.fsm :as fsm]))

(defn- form-attr?
  [k]
  (when (keyword? k)
    (= "attr" (namespace k))))

(defn- done=
  "The number of form attributes contained in the entity must be greater
  than or equal to n. Not a realistic validation, but focus here is
  not on validation."
  [n]
  (fn [[e]]
    (->> (keys e)
         (filter form-attr?)
         (count)
         (<= n))))

(fsm/reg-fsm ::workflow-a
  (fn [self w1 w2]
    {:ref self
     :or  ::step-1
     :fsm {::step-1 {[::done w1] ::step-2}
           ::step-2 {[::done w2] ::done}
           ::done   nil}}))

(fsm/reg-fsm ::workflow-b
  (fn [self f1 f2 f3 f4]
    {:ref self
     :or  ::step-1
     :fsm {::step-1 {[::done f1] ::step-2}
           ::step-2 {[::done f2] ::step-3}
           ::step-3 {[::done f3] ::step-4}
           ::step-4 {[::done f4] {:to       ::done
                                  :dispatch [::done self]}}
           ::done   nil}}))

(fsm/reg-fsm ::workflow-c
  (fn [self f1 f2 f3]
    {:ref self
     :or  ::step-1
     :fsm {::step-1 {[::done f1] ::step-2}
           ::step-2 {[::done f2] ::step-3}
           ::step-3 {[::done f3] {:to       ::done
                                  :dispatch [::done self]}}
           ::done   nil}}))

(fsm/reg-fsm ::form
  (fn [self n]
    {:ref self
     :fsm {nil    {[::select self] {:to       ::done
                                    :when     (done= n)
                                    :pull     [self]
                                    :dispatch [::done self]}}
           ::done nil}}))

(f/reg-no-op ::done)

(f/reg-event-db ::select
  (fn [db [_ form attr]]
    (db/update-inn db [form attr] not)))

(f/reg-pull ::selected?
  (fn [form attr]
    [attr form]))

(f/reg-pull ::done?
  (fn [self]
    [::fsm/state self])
  (fn [state]
    (= state ::done)))
