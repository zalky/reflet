(ns reflet.client.ui.workflow
  (:require [goog.string :as gstr]
            [goog.string.format]
            [reflet.client.ui.workflow.impl :as impl]
            [reflet.core :as f]
            [reflet.debug.glyphs :as g]))

;;;; Utility

(def option-labels
  ["A" "B" "C" "D"])

(defn- stroke
  [props]
  (-> {:stroke-width "2px"}
      (merge props)
      (g/stroke)))

(defn- checked
  []
  [:path (stroke {:d "M 8,10 L 10,12 17,5"})])

(defn- check
  [{:keys [done on-click]
    :as   props}]
  [:div {:class    "workflow-check"
         :on-click on-click}
   [:svg {:view-box "0 0 20 20"}
    [:circle (stroke {:cx 10
                      :cy 10
                      :r  6})]
    (when done (checked))]])

(defn- step
  [{:keys [done active on-click]
    :as   props}]
  [:div {:class    "workflow-step"
         :on-click on-click}
   [:svg {:view-box "0 0 20 20"}
    (if done
      (checked)
      [:circle (stroke {:cx   10
                        :cy   10
                        :r    (if active 6 3)
                        :fill "currentColor"})])]])

(defn- line
  []
  [:div {:class "workflow-line"}
   [:svg {:view-box "0 0 80 20"}
    [:path (stroke {:d "M 5,10 L 75,10"})]]])

(defn- grid-template
  [items]
  (->> (count items)
       (dec)
       (gstr/format "repeat(%d, 20px 80px) 20px")
       (hash-map :grid-template-columns)))

(defn- progress-step
  [{:keys [active label]
    :as   item}]
  [:div {:class "workflow-progress-step"}
   [step item]
   [:div {:class (when active "active")}
    label]])

(defn- progress
  [items]
  [:div {:class "workflow-progress"
         :style (grid-template items)}
   (->> items
        (map progress-step)
        (interpose [line])
        (map-indexed #(with-meta %2 {:key %1}))
        (doall))])

;;;; Workflows

(defn- form-entry
  [i self]
  (let [attr      (keyword "attr" i)
        selected? (f/sub [::impl/selected? self attr])
        on-click  #(f/disp [::impl/select self attr])]
    [:<> {:key i}
     [:div {:class "workflow-label"}
      (nth option-labels i)]
     [check {:on-click on-click
             :done     @selected?}]]))

(defn- form
  [{:keys [self required total]}]
  {:pre [(number? total)]}
  @(f/sub [::impl/form self required])
  [:div {:class "workflow-form"}
   [:div {:class "workflow-required"}
    [:span "Choose"]
    [:span required]
    [:span "/"]
    [:span total]]
   (doall
    (for [i (range total)]
      (form-entry i self)))])

(defmulti workflow-b
  (fn [state _] state)
  :default ::impl/step-1)

(defmethod workflow-b ::impl/step-1
  [_ {:keys [::f1]}]
  [form {:self     f1
         :required 2
         :total    3}])

(defmethod workflow-b ::impl/step-2
  [_ {:keys [::f2]}]
  [form {:self     f2
         :required 2
         :total    4}])

(defmethod workflow-b ::impl/step-3
  [_ {:keys [::f3]}]
  [form {:self     f3
         :required 1
         :total    3}])

(defmethod workflow-b ::impl/step-4
  [_ {:keys [::f4]}]
  [form {:self     f4
         :required 1
         :total    1}])

(defmethod workflow-b ::impl/done
  [_ _]
  [:div {:class "workflow-done"}
   "Done!"])

(defmulti workflow-c
  (fn [state _] state)
  :default ::impl/step-1)

(defmethod workflow-c ::impl/step-1
  [_ {:keys [::f1]}]
  [form {:self     f1
         :required 2
         :total    2}])

(defmethod workflow-c ::impl/step-2
  [_ {:keys [::f2]}]
  [form {:self     f2
         :required 1
         :total    4}])

(defmethod workflow-c ::impl/step-3
  [_ {:keys [::f3]}]
  [form {:self     f3
         :required 2
         :total    3}])

(defmethod workflow-c ::impl/done
  [_ _]
  [:div {:class "workflow-done"}
   "Done!"])

(defmulti workflow-a
  (fn [state _] state)
  :default ::impl/step-1)

(defmethod workflow-a ::impl/step-1
  [_ props]
  (f/with-ref {:cmp/uuid [::b ::f1 ::f2 ::f3 ::f4]
               :in       props}
    (let [state @(f/sub [::impl/workflow-b b f1 f2 f3 f4])]
      [:<>
       [progress [{:active (= state ::impl/step-1)
                   :done   @(f/sub [::impl/done? f1])
                   :label  "Form 1"}
                  {:active (= state ::impl/step-2)
                   :done   @(f/sub [::impl/done? f2])
                   :label  "Form 2"}
                  {:active (= state ::impl/step-3)
                   :done   @(f/sub [::impl/done? f3])
                   :label  "Form 3"}
                  {:active (= state ::impl/step-4)
                   :done   @(f/sub [::impl/done? f4])
                   :label  "Form 4"}]]
       [workflow-b state props]])))

(defmethod workflow-a ::impl/step-2
  [_ props]
  (f/with-ref {:cmp/uuid [::c ::f1 ::f2 ::f3]
               :in       props}
    (let [state @(f/sub [::impl/workflow-c c f1 f2 f3])]
      [:<>
       [progress [{:active (= state ::impl/step-1)
                   :done   @(f/sub [::impl/done? f1])
                   :label  "Form 1"}
                  {:active (= state ::impl/step-2)
                   :done   @(f/sub [::impl/done? f2])
                   :label  "Form 2"}
                  {:active (= state ::impl/step-3)
                   :done   @(f/sub [::impl/done? f3])
                   :label  "Form 3"}]]
       [workflow-c state props]])))

(defmethod workflow-a ::impl/done
  [_ props]
  [:div {:class "workflow-done"}
   "Done!"])

(defn workflow
  [props]
  (f/with-ref {:cmp/uuid [::a ::b ::c]
               :in       props}
    (let [state @(f/sub [::impl/workflow-a a b c])]
      [:div {:class "workflow"}
       (when-not (= ::impl/done state)
         [progress [{:active (= state ::impl/step-1)
                     :done   @(f/sub [::impl/done? b])
                     :label  "Workflow B"}
                    {:active (= state ::impl/step-2)
                     :done   @(f/sub [::impl/done? c])
                     :label  "Workflow C"}]])
       [workflow-a state props]])))
