(ns reflet.fixtures
  (:require [day8.re-frame.test :as t]))

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
