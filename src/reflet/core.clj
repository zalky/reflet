(ns reflet.core
  (:require [cinch.core :as util]
            [clojure.spec.alpha :as s]
            [clojure.walk :as w]
            [re-frame.core :as f]
            [reagent.core :as r]
            [reflet.db :as db]
            [reflet.debug :as d]
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
  `(not-empty
    (if (or (false? (:transient ~meta))
            *force-persistent-refs*)
      (dissoc ~meta :transient)
      (assoc ~meta :transient true))))

(defn- mounted-random-ref
  [refs k id-attr {:keys [meta]}]
  `(let [m#   ~(parse-meta meta)
         ref# (db/random-ref ~id-attr m# ~k)]
     (vswap! ~refs assoc ~k ref#)
     (db/mount-ref! ref#)
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
  [refs parsed env {:keys [in] :as opts}]
  (let [keys        (mapv :key parsed)
        id-attrs    (mapv :id-attr parsed)
        given-syms  (mapv :sym parsed)
        unique-syms (mapv gensym given-syms)
        props-sym   (or in (gensym))]
    [props-sym   (if (bound-local? env props-sym) props-sym {})
     unique-syms (get-refs props-sym refs keys id-attrs opts)
     given-syms  unique-syms
     props-sym   `(->> ~(zipmap keys unique-syms)
                       (merge ~props-sym))]))

(defn- with-ref-cleanup
  "Clean up clause.

  This runs during :component-will-unmount, which technically happens
  before the :component-did-mount of subsequent lifecycles. However,
  the first render of the next cycle happens
  before :component-will-unmount is run, and in order for this to
  happen, the first props must be computed
  before :component-will-unmount. This means that the initial props
  that :component-did-mount sees can never actually see these cleanup
  changes regardless of whether they are dispatched asynchronously or
  synchronously. Because of these concurrency considerations there are
  two golden rules of using transient refs:

  1. Do not use transient refs as joins (map values or link values)
     in db writes

  2. Do not pass them to contexts outside of the `with-ref` in which
     they were made"
  [refs]
  (list
   'finally
   `(doseq [[k# ref#] (deref ~refs)]
      (when (and ref# (db/transient? ref#))
        (db/unmount-ref! ref#)
        (f/dispatch [::with-ref-cleanup ref#])
        (when (= k# ::debug-id)
          (f/dispatch [::d/untap ref#]))))))

(defn- component-name
  []
  `(reagent.impl.component/component-name
    (r/current-component)))

(defn- debug?
  [{:keys [debug]
    :or   {debug true}}]
  `(and ~debug d/*debug*))

(defn- debug-id
  [refs opts]
  `(when ~(debug? opts)
     (let [r# (db/random-ref :debug/uuid {:transient true})]
       (vswap! ~refs assoc ::debug-id r#)
       (db/mount-ref! r#)
       r#)))

(defn- get-opts
  [bindings]
  (util/split-keys bindings [:in :meta :debug]))

(defn- wrap-debug
  [refs target d-id body opts]
  `(if ~(debug? opts)
     (let [p# {:debug/id   ~d-id
               :debug/uuid (second ~d-id)
               :debug/name ~(component-name)
               :debug/refs (deref ~refs)}]
       [:<>
        (d/*debug* ~target p#)
        (do ~@body)])
     (binding [d/*debug* false]
       (do ~@body))))

(defmacro with-ref
  "Generates entity references. Optionally rebinds props attributes,
  and dispatches entity cleanup. See the Reflet wiki for motivation,
  usage and other documentation."
  [bindings & body]
  (let [[opts unparsed] (get-opts bindings)
        parsed          (s/conform ::rs/bindings unparsed)
        refs            (gensym)
        d-id            (gensym)
        target          (gensym)
        env             &env]
    `(r/with-let [~refs   (volatile! {})
                  ~d-id   ~(debug-id refs opts)
                  ~target (r/atom nil)]
       (let ~(if (= parsed ::s/invalid)
               (throw-parse-err! unparsed)
               (bind-refs refs parsed env opts))
         ~(wrap-debug refs target d-id body opts))
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

(defmacro once
  [forms & body]
  `(r/with-let [_# ~forms]
     ~@body))
