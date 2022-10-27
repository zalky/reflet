(ns reflet.core
  (:require [cinch.core :as util]
            [clojure.spec.alpha :as s]
            [clojure.walk :as w]
            [re-frame.core :as f]
            [reagent.core :as r]
            [reagent.ratom :as r*]
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
  [props-sym refs parsed env {:keys [in] :as opts}]
  (let [keys        (mapv :key parsed)
        id-attrs    (mapv :id-attr parsed)
        given-syms  (mapv :sym parsed)
        unique-syms (mapv gensym given-syms)]
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
  [refs context]
  (list
   'finally
   `(when-let [ref# (:debug-id ~context)]
      (db/unmount-ref! ref#)
      (disp [::with-ref-cleanup ref#])
      (disp [::d/untap ref#]))

   `(doseq [[k# ref#] (deref ~refs)]
      (when (and ref# (db/transient? ref#))
        (db/unmount-ref! ref#)
        (disp [::with-ref-cleanup ref#])))))

(defn- env-name
  [env]
  (-> env :fn-scope first :name str))

(defn- env-ns
  [env]
  (-> env :fn-scope first :info :ns str))

(defn- env-line
  [env]
  (-> env :line))

(defn- debug?
  [{:keys [debug]
    :or   {debug true}}]
  `(and (r*/reactive?) ~debug d/*debug*))

(defn- wrap-debug
  [props-sym context env body opts]
  `(if ~(debug? opts)
     (let [p# {:debug/type  :debut.type/tap
               :debug/id    (second (:debug-id ~context))
               :debug/fn    (symbol ~(env-ns env) ~(env-name env))
               :debug/line  ~(env-line env)
               :debug/props ~props-sym}]
       [:<>
        (d/*debug* p# (:target ~context))
        (do ~@body)])
     (do ~@body)))

(defn- component-name
  []
  `(reagent.impl.component/component-name
    (r/current-component)))

(defn- debug-id
  "Produces a debug id that is unique within a dom tree, but not across
  trees with the same topology."
  [env]
  `(let [^clj c# r*/*ratom-context*
         g#      (or (.-withRefGeneration c#) 0)
         ns#     ~(env-ns env)
         n#      (str ~(env-name env) "$" g#)
         r#      [:debug/id (keyword ns# n#)]]
     (set! (.-withRefGeneration c#) (inc g#))
     (db/mount-ref! r#)
     r#))

(defn debug-context
  [env opts]
  `(when ~(debug? opts)
     {:target   (r/atom nil)
      :debug-id ~(debug-id env)}))

(defn- get-opts
  [bindings]
  (util/split-keys bindings [:in :meta :debug]))

(defmacro with-ref
  "Generates entity references. Optionally rebinds props attributes,
  and dispatches entity cleanup. See the Reflet wiki for motivation,
  usage and other documentation."
  [bindings & body]
  (let [[opts unparsed] (get-opts bindings)
        parsed          (s/conform ::rs/bindings unparsed)
        props           (or (:in opts) (gensym))
        context         (gensym)
        refs            (gensym)
        env             &env]
    `(r/with-let [~refs    (volatile! {})
                  ~context ~(debug-context env opts)]
       (let ~(if (= parsed ::s/invalid)
               (throw-parse-err! unparsed)
               (bind-refs props refs parsed env opts))
         ~(wrap-debug props context env body opts))
       ~(with-ref-cleanup refs context))))

(defmacro with-ref*
  "Like with-ref but debug disabled. For implementing the debug UI."
  [bindings & body]
  `(with-ref ~(assoc bindings :debug false)
     ~@body))

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
  [& forms]
  (let [conditional (if (second forms) (first forms) true)
        expr        (or (second forms) (first forms))]
    `(r/with-let [r# (volatile! true)]
       (when (and ~conditional (deref r#))
         (vreset! r# false)
         ~expr))))
