(ns reflet.debugger
  (:require [clojure.spec.alpha :as s]
            [reflet.core :as f]
            [reflet.ref-spec :as rs]))

(defmacro with-ref
  [bindings & body]
  (let [[opts unparsed] (f/get-opts bindings)
        parsed          (s/conform ::rs/bindings unparsed)
        refs            (gensym)
        env             &env]
    `(reagent.core/with-let [~refs (volatile! {})]
       (let ~(if (= parsed ::s/invalid)
               (f/throw-parse-err! unparsed)
               (f/bind-refs refs parsed env opts))
         ~@body)
       ~(f/with-ref-cleanup refs))))
