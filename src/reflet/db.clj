(ns reflet.db
  (:require [re-frame.interop :as interop]
            [re-frame.trace :as trace]
            [reagent.ratom :as r]))

(defmacro traced-reaction
  [query-v reaction-fn & [dispose-fn]]
  `(let [r-id# (atom nil)
         r#    (r/make-reaction
                 (fn []
                   (trace/with-trace
                     {:operation (first ~query-v)
                      :op-type   :sub/run
                      :tags      {:query-v  ~query-v
                                  :reaction @r-id#}}
                     (let [res# (~reaction-fn)]
                       (trace/merge-trace! {:tags {:value res#}})
                       res#)))
                 :on-dispose ~dispose-fn)]
     (->> r#
          (interop/reagent-id)
          (reset! r-id#))
     r#))
