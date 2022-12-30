(ns reflet.fixtures
  (:require [day8.re-frame.test :as t]
            [reagent.ratom :as r]))

(defmacro run-test-sync
  [& body]
  `(t/run-test-sync
    (reflet.fixtures/base-fixtures
     (fn [] ~@body))))

(defmacro run-test-async
  [& body]
  `(t/run-test-async
    (reflet.fixtures/base-fixtures
     (fn [] ~@body))))

(defmacro fake-reactive-context
  "Simulates a fake reactive context. Should only be used when you
  want to test things like cleanup and other properties of transient
  refs."
  [& body]
  `(let [r# {}]
     (set! (.-ratomGeneration r#) 0)
     (binding [r/*ratom-context* r#]
       ~@body)))
