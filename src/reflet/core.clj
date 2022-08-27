(ns reflet.core
  (:require [clojure.spec.alpha :as s]
            [clojure.walk :as w]
            [re-frame.core :as f]
            [reagent.ratom :as r]
            [reflet.ref-spec :as ref-spec]
            [reflet.util :as util]))

(defn- parse-meta
  [meta]
  (not-empty
   (if (and (contains? meta :transient)
            (not (:transient meta)))
     (dissoc meta :transient)
     (assoc meta :transient true))))

(defn- new-transient-ref
  [k attr bindings {:keys [meta]}]
  (let [meta* (parse-meta meta)]
    `(let [ref# (cond-> (~'reflet.core/*random-ref* ~attr ~k)
                  ~meta* (update 1 with-meta ~meta*))]
       (if (:transient ~meta*)
         (vswap! ~bindings assoc-in [:transient ~k] ref#)
         (vswap! ~bindings assoc-in [:persistent ~k] ref#))
       (reflet.db/mount-ref! ref#)
       ref#)))

(defn- bind-refs
  "Produces local entity bindings. These must be cleaned up."
  [props-sym bindings keys attrs opts]
  (let [k    (gensym)
        attr (gensym)]
    `(map (fn [~k ~attr]
            (let [e# (get ~props-sym ~k)]
              (if (nil? e#)
                (or (get-in (deref ~bindings) [:persistent ~k])
                    (get-in (deref ~bindings) [:transient ~k])
                    ~(new-transient-ref k attr bindings opts))
                e#)))
          ~keys
          ~attrs)))

(defn- bound-local?
  [env sym]
  (let [{:keys [locals]} env]
    (->> sym
         (name)
         (symbol)
         (contains? locals))))

(defn- bind
  "Binds entity refs to symbols. Two sets of symbols are bound, the user
  defined symbols, and a set of symbols guaranteed to be unique in
  order to properly bound props."
  [props-sym bindings parsed opts env]
  (let [keys        (mapv :key parsed)
        attrs       (mapv :attr parsed)
        user-syms   (mapv :sym parsed)
        unique-syms (mapv gensym user-syms)]
    [props-sym   (if (bound-local? env props-sym) props-sym {})
     unique-syms (bind-refs props-sym bindings keys attrs opts)
     user-syms   unique-syms
     props-sym   `(merge ~props-sym ~(zipmap keys unique-syms))]))

(defn- throw-parse-err!
  [unparsed]
  (throw
   (ex-info
    (s/explain ::ref-spec/binding-map unparsed)
    unparsed)))

(defn- with-ref-cleanup
  "Clean up clause."
  [bindings]
  (list 'finally
        `(doseq [ref# (vals (:transient (deref ~bindings)))]
           (when ref#
             (reflet.db/unmount-ref! ref#)
             ;; Note: dispatch-sync causes errors in Karma test
             ;; runners. However, app state can be cleaned up async,
             ;; so prefer regular dispatch.
             (f/dispatch [::with-ref-cleanup ref#])))))

(defmacro with-ref
  "Generates entity references. Optionally rebinds props attributes,
  and dispatches entity cleanup. See the Aria wiki for motivation,
  usage and other documentation."
  [bindings & body]
  (let [[opts unparsed]  (util/split-keys bindings [:in :meta])
        {props-sym* :in} opts
        env              &env
        parsed           (s/conform ::ref-spec/binding-map unparsed)
        props-sym        (or props-sym* (gensym))
        bindings         (gensym)]
    `(reagent.core/with-let [~bindings (volatile! {})]
       (let ~(if (= parsed ::s/invalid)
               (throw-parse-err! unparsed)
               (bind props-sym bindings parsed opts env))
         ~@body)
       ~(with-ref-cleanup bindings))))

(defn- no-eval-keywords
  [expr]
  (w/postwalk
   (fn [x]
     (if (and (list? x) (keyword? (first x)))
       (cons 'list x)
       x))
   expr))

(defmacro reg-pull
  "Version of reflet.core/reg-pull that prevents keyword
  evaluation. This is a convenience to facilitate sync expressions."
  [& [id [_ bindings & spec] result-fn]]
  (concat
   `(reg-pull* ~id
     (fn ~bindings ~@(no-eval-keywords spec)))
   (when result-fn [result-fn])))

(defmacro resubscribe
  "Like re-frame.core/subscribe, but reactively updates the underlying
  subscription if the arguments change. The goal is to leverage the
  re-frame subscription cash, but allow for it to be used inside a
  with-let macro."
  [expr]
  `(reflet.core/IResubscribe. (fn [] (f/subscribe ~expr))))
