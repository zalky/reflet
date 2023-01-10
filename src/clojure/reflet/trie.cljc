(ns reflet.trie
  "Trie for longest prefix matching.

  Ideally this would implement clojure.lang interfaces."
  (:refer-clojure :exclude [merge])
  (:require [cinch.core :as util]))

(defprotocol Trie
  (add [obj prefix v])
  (match [obj prefix])
  (merge [obj other]))

(declare ->PrefixTrie)

(defn trie
  ([] (->PrefixTrie nil nil))
  ([node] (->PrefixTrie node nil))
  ([node child] (->PrefixTrie node child)))

(defn- upsert
  [obj prefix v]
  (-> (or obj (trie))
      (add prefix v)))

(defn- add*
  [obj v]
  (assoc obj :node v))

(defn- match*
  [result {c :child} [k & more]]
  (if k
    (-> c
        (get-in [k :node] result)
        (recur (get c k) more))
    result))

(defrecord PrefixTrie [node child]
  Trie
  (add [obj [k & more] v]
    (if k
      (update-in obj [:child k] upsert more v)
      (add* obj v)))

  (match [obj prefix]
    (match* nil obj prefix))

  (merge [obj other]
    (util/merge-deep obj other)))

(extend-protocol Trie
  nil
  (add [obj prefix v]
    (add (trie) prefix v))

  (match [obj other] nil)

  (merge [obj other] other))
