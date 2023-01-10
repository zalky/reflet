(ns reflet.core
  (:require [cinch.core :as util]
            [clojure.spec.alpha :as s]
            [clojure.walk :as w]
            [re-frame.core :as f]
            [reagent.core :as r]
            [reagent.ratom :as r*]
            [reflet.db :as db]
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
  [{:keys [meta persist]}]
  `(not-empty
    (if (or ~persist (not (r*/reactive?)))
      (dissoc ~meta :transient)
      (assoc ~meta :transient true))))

(defn- mounted-random-ref
  [refs k id-attr opts]
  `(let [m#   ~(parse-meta opts)
         ref# (db/random-ref ~id-attr m# ~k)]
     (when (:transient m#)
       (db/mount-ref! ref#))
     (vswap! ~refs assoc ~k ref#)
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

  This dispatches during :component-will-unmount, which technically
  happens before the :component-did-mount of subsequent
  lifecycles. However, because of the dispatch the actual cleanup
  side-effect will not happen until sometime after that. So for all
  intents and purposes, it is best to treat cleanup as an asynchronous
  process that happens some time after unmount."
  [refs context]
  (list
   'finally
   `(do
      (when-let [ref# (some-> ~context :debug-id deref)]
        (disp [::cleanup ref#]))
      (doseq [[_# ref#] (deref ~refs)]
        (when (and ref# (db/transient? ref#))
          (disp [::cleanup ref#]))
        (when (some-> ~context :debug-id deref)
          (disp [::db/untrace-event ref#]))))))

(defn- env-namespace
  [env]
  (-> env :ns :name str not-empty))

(defn- debug?
  [{:keys [debug]
    :or   {debug true}}]
  `(and (r*/reactive?) ~debug db/tap-fn))

(defn- component-name
  []
  `(reagent.impl.component/component-name
    (r/current-component)))

(defn- wrap-debug
  [props-sym context env opts body]
  `(let [r# (do ~@body)]
     (if ~(debug? opts)
       (let [p# {:debug/type  :debut.type/tap
                 :debug/name  ~(component-name)
                 :debug/ns    ~(env-namespace env)
                 :debug/line  ~(:line env)
                 :debug/props ~props-sym}]
         [:<> (db/tap-fn p# ~context) r#])
       r#)))

(defn- with-ref-gen
  "Produces a debug id that is unique within a DOM tree, but not across
  trees with the same topology. This allows us to track props across
  hot restarts."
  [env]
  `(let [^clj c# r*/*ratom-context*
         gen#    (or (.-withRefGeneration c#) 0)]
     (set! (.-withRefGeneration c#) (inc gen#))
     gen#))

(defn debug-context
  [env opts]
  `(when ~(debug? opts)
     {:target   (r/atom nil)
      :debug-id (atom nil)
      :gen      ~(with-ref-gen env)}))

(defn- get-opts
  [bindings]
  (util/split-keys bindings [:in :meta :persist :debug]))

(defmacro with-ref
  "Generates entity references. Optionally rebinds props attributes,
  and dispatches entity cleanup. See the Reflet wiki for motivation,
  usage and documentation of other options."
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
         ~(wrap-debug props context env opts body))
       ~(with-ref-cleanup refs context))))

(defmacro with-ref*
  "Like with-ref but with debug implicitly disabled. Used to implement
  the debug UI. Not for public use: throws an error if used outside of
  a reflet.debug.* namespace."
  [bindings & body]
  (let [ns   (env-namespace &env)
        line (:line &env)]
    (when-not (re-find #"^reflet.debug" ns)
      (-> "with-ref* used outside reflet debug namespace"
          (ex-info {:ns ns :line line})
          (throw)))
    `(with-ref ~(assoc bindings :debug false)
       ~@body)))

(defn- side-effect?
  [x]
  (and (list? x) (not (symbol? (first x)))))

(defn- no-eval-keywords
  [expr]
  (w/postwalk
   (fn [x]
     (if (side-effect? x)
       (cons 'list x)
       x))
   expr))

(defmacro reg-pull
  "Registers a named pull query. Semantics are similar to Datomic pull,
  but with link and attribute queries. See wiki for full
  details. Macro prevents keyword evaluation. This is a convenience to
  facilitate sync expressions."
  [& [id [_ bindings & spec] result-fn]]
  (concat
   `(reg-pull-impl ~id
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
