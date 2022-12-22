(ns reflet.client.ui.form
  (:require [reflet.core :as f]))

(defn form
  [props]
  (f/with-ref {:cmp/uuid [form/self]
               :el/uuid  [form/el]
               :in       props}
    [:div {:class "form"}
     "Form"]))
