(ns reflet.core
  (:require [cinch.core :as util]
            [clojure.spec.alpha :as s]
            [clojure.walk :as w]
            [re-frame.core :as f]
            [reagent.ratom :as r]
            [reflet.ref-spec :as rs]))

(defn- throw-parse-err!
  [unparsed]
  (throw
   (ex-info
    (s/explain ::rs/binding-map unparsed)
    unparsed)))

(defn- bound-local?
  [env sym]
  (let [{:keys [locals]} env]
    (->> sym
         (name)
         (symbol)
         (contains? locals))))

(defn- parse-meta
  "All newly created refs are transient by default."
  [meta]
  (not-empty
   (if (false? (:transient meta))
     (dissoc meta :transient)
     (assoc meta :transient true))))

(defn- mounted-random-ref
  [refs k id-attr {:keys [meta]}]
  `(let [m#   ~(parse-meta meta)
         ref# (reflet.db/random-ref ~id-attr m# ~k)]
     (vswap! ~refs assoc ~k ref#)
     (reflet.db/mount-ref! ref#)
     ref#))

(defn- get-refs
  [props-sym refs keys id-attrs opts]
  (let [k       (gensym)
        id-attr (gensym)]
    `(map (fn [~k ~id-attr]
            (let [v# (get ~props-sym ~k)]
              (if (nil? v#)
                (or (get (deref ~refs) ~k)
                    ~(mounted-random-ref refs k id-attr opts))
                v#)))
          ~keys
          ~id-attrs)))

(defn- bind-refs
  "Two sets of symbols are bound, the symbols given by the bindings, and
  a set guaranteed to be unique in order to properly bound
  props."
  [props-sym refs parsed opts env]
  (let [keys        (mapv :key parsed)
        id-attrs    (mapv :id-attr parsed)
        given-syms  (mapv :sym parsed)
        unique-syms (mapv gensym given-syms)]
    [props-sym   (if (bound-local? env props-sym) props-sym {})
     unique-syms (get-refs props-sym refs keys id-attrs opts)
     given-syms  unique-syms
     props-sym   `(merge ~props-sym
                         ~(zipmap keys unique-syms))]))

(defn- with-ref-cleanup
  "Clean up clause."
  [refs]
  (list 'finally
        `(doseq [ref# (->> ~refs
                           (deref)
                           (vals)
                           (filter reflet.db/transient?))]
           (when ref#
             (reflet.db/unmount-ref! ref#)
             ;; Note: dispatch-sync causes errors in Karma test
             ;; runners. However, app state can be cleaned up async,
             ;; so prefer regular dispatch.
             (f/dispatch [::with-ref-cleanup ref#])))))

(defmacro with-ref
  "Generates entity references. Optionally rebinds props attributes,
  and dispatches entity cleanup. See the Reflet wiki for motivation,
  usage and other documentation."
  [bindings & body]
  (let [[opts unparsed]  (util/split-keys bindings [:in :meta])
        {props-sym* :in} opts
        env              &env
        parsed           (s/conform ::rs/binding-map unparsed)
        props-sym        (or props-sym* (gensym))
        refs             (gensym)]
    `(reagent.core/with-let [~refs (volatile! {})]
       (let ~(if (= parsed ::s/invalid)
               (throw-parse-err! unparsed)
               (bind-refs props-sym refs parsed opts env))
         ~@body)
       ~(with-ref-cleanup refs))))

(defn- no-eval-keywords
  [expr]
  (w/postwalk
   (fn [x]
     (if (and (list? x) (keyword? (first x)))
       (cons 'list x)
       x))
   expr))

(defmacro reg-pull
  "Registers a named pull query. Semantics are similar to datomic pull,
  but with link and attribute queries. See wiki for full
  details. Macro prevents keyword evaluation. This is a convenience to
  facilitate sync expressions."
  [& [id [_ bindings & spec] result-fn]]
  (concat
   `(reg-pull* ~id
     (fn ~bindings ~@(no-eval-keywords spec)))
   (when result-fn [result-fn])))

