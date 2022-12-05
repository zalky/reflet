(ns reflet.css.bundle
  (:require [axle.core :as watch]
            [taoensso.timbre :as log]))

(def css-path
  "resources/debug.css")

(def bundle-path
  "src/clojure/reflet/css/bundled.cljs")

(def bundled-css-string
  "(ns reflet.css.bundled) (def css %s)")

(defn bundle-css
  [_ e]
  (->> css-path
       (slurp)
       (prn-str)
       (format bundled-css-string)
       (spit bundle-path))
  (log/info "Bundled reflet.css.bundled/css"))

(defn bundle
  [_]
  (log/info "Bundling css...")
  (watch/watch!
   {:paths   [css-path]
    :handler bundle-css})
  {:runway/block true})
