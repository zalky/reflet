(ns reflet.db
  (:require [re-frame.interop :as interop]
            [re-frame.trace :as trace]
            [reagent.ratom :as r]))

(defmacro traced-reaction
  [q-ref query-v reaction-fn & [dispose-fn]]
  `(let [id# (atom nil)
         r#  (r/make-reaction
               (fn []
                 (trace/with-trace
                   {:operation (first ~query-v)
                    :op-type   :sub/run
                    :tags      {:query-v  ~query-v
                                :reaction @id#}}
                   (let [res# (~reaction-fn)]
                     (trace/merge-trace! {:tags {:value res#}})
                     res#)))
               :on-dispose ~dispose-fn)]
     (when ~q-ref
       (set! (.-reflet-query-ref r#) ~q-ref))
     (->> r#
          (interop/reagent-id)
          (reset! id#))
     r#))
