(ns reflet.fixtures
  (:require [day8.re-frame.test :as t]
            [reagent.ratom :as r]
            [reflet.db :as db]))

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
  "Simulates a fake reactive context for testing with-ref in specific
  scenarios, like cleanup and other properties of transient refs. Has
  not been designed to work in any other context."
  [& body]
  `(let [r# {}
         g# (swap! fake-ratom-generation inc)]
     (set! (.-ratomGeneration r#) g#)
     (binding [r/*ratom-context* r#]
       (with-redefs [db/tap-fn false]
         (let [result# (do ~@body)]
           (doseq [[_# r#] ^clj (.-reagReactionCache r/*ratom-context*)]
             (r/dispose! r#))
           result#)))))
