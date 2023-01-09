(ns reflet.css.bundle
  (:require [axle.core :as watch]
            [clojure.string :as str]))

(def css-path
  "resources/debug.css")

(def bundle-path
  "src/clojure/reflet/css/bundled.cljs")

(def bundled-css-string
  "(ns reflet.css.bundled) (def css %s)")

(defn strip-source-map
  [s]
  (str/replace s #"/\*# sourceMappingURL=debug\.css\.map \*/" ""))

(defn- bundle-css
  [_ e]
  (->> css-path
       (slurp)
       (prn-str)
       (strip-source-map)
       (format bundled-css-string)
       (spit bundle-path))
  (println "reflet - Bundled reflet.css.bundled/css"))

(defn bundle
  [_]
  (println "reflet - Bundling css...")
  (watch/watch!
   {:paths   [css-path]
    :handler bundle-css})
  {:runway/block true})
