(ns wave.type.cmn.ctx
  (:require
   [crypticbutter.snoop :refer [>defn]]))

(def LexicalBindingSch
  [:map])

(def GlobalBindingSch
  [:map])

(def Schema
  [:map
   [:lexical-bindings
    [:map-of some? some?]]
   [:global-bindings
    [:map]]])

(defn unwind-lexical-ctx [ctx]
  (assoc ctx :lexical-bindings
         (-> ctx :lexical-bindings :parent)))

(defn wind-lexical-ctx [{:keys [lexical-bindings] :as ctx}]
  (assoc ctx :lexical-bindings
         (assoc lexical-bindings :parent lexical-bindings)))

(>defn lookup-global [ctx sym]
  [Schema some? '=> :any]
  (get-in (:global-bindings ctx) [(namespace sym) (name sym)]))

(>defn lookup-lexical [ctx sym]
  [Schema some? '=> :any]
  (get (:lexical-bindings ctx) sym))

(defn lookup-sym [ctx sym]
  (or (some->> (lookup-lexical ctx sym) (vector :lexical))
      (some->> (lookup-global ctx sym) (vector :var))))
