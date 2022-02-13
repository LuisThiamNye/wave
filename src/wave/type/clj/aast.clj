(ns wave.type.clj.aast
  (:require
   [malli.core :as m]
   [malli.util :as mu]
   [com.rpl.specter :as sp]
   [wave.type.cmn.ctx :as ctx]
   [crypticbutter.snoop :refer [>defn]]
   [wave.type.cmn.aast :as aast]))

(def NodeIdSch uuid?)
(def AnnotationSch [:map [:id NodeIdSch]])

(def EdnNodeSch :any)
(def LiteralTypes [:enum :vector :list :map :set :number :keyword :string :nil :bool :symbol])
(def LiteralTypesWithChildren [:enum :vector :list :map :set])
(def PartialNodeSch
  [:map
   [:literal-type LiteralTypes]
   [:id {:optional true} NodeIdSch]
   [::expr EdnNodeSch]])
(def NodeSchema [:and
                 [:or
                  [:map
                   [:literal-type LiteralTypesWithChildren]
                   [:children [:vector NodeIdSch]]]
                  [:map
                   [:literal-type [:and keyword?
                                   [:not LiteralTypesWithChildren]]]]]
                 [:map
                  [:id {:optional true} NodeIdSch]
                  [::expr EdnNodeSch]]])

(def CtxSchema
  (mu/merge ctx/Schema
            [:map
             [:annotations [:vector AnnotationSch]]
             [:nodes [:map-of NodeIdSch PartialNodeSch]]]))

#_(>defn get-node-ann [ctx node]
  [CtxSchema NodeSchema '=> [:maybe AnnotationSch]]
  (get-in ctx [:annotations (:id node)]))

(>defn edn->aast [code]
  [EdnNodeSch '=> PartialNodeSch]
  {::expr code
   :literal-type (cond
                   (seq? code) :list
                   (vector? code) :vector
                   (map? code) :map
                   (symbol? code) :symbol
                   (set? code) :set
                   (number? code) :number
                   (string? code) :string
                   (keyword? code) :keyword
                   (nil? code) :nil
                   (boolean? code) :bool)
   :id (random-uuid)})

(defn annotated? [code]
  (contains? (meta code) ::ann))

#_(defn ensure-aast [code]
  (if (annotated? code)
    code
    (edn->aast code)))

(defn node-id [node] (:id node))
(>defn get-node [ctx nid]
  [:any NodeIdSch '=> NodeSchema]
  (get-in ctx [:nodes nid]))

(>defn reg-node
  ([ctx expr] (reg-node ctx expr nil))
  ([ctx expr nodeinfo]
   [CtxSchema EdnNodeSch [:maybe map?]
    '=> [:tuple CtxSchema NodeIdSch]]
   (let [node (edn->aast expr)
         id (node-id node)]
     [(update ctx :nodes assoc id
              (assoc (merge node nodeinfo)
                     ::expr expr
                     :id (node-id node)))
      id])))

(>defn add-children [ctx nid children]
  [CtxSchema NodeIdSch [:vector NodeIdSch]
   '=> CtxSchema]
  (assoc-in ctx [:nodes nid :children] children))

(>defn annotate-extra [ctx nid n']
  [CtxSchema NodeIdSch map?
   '=> CtxSchema]
  (let [n1 (get-node ctx nid)
        n2 (update n1 :errors into (:errors n'))
        ctx (sp/setval [(sp/keypath :nodes nid)]
                       n2 ctx)]
    ctx))

(defn get-in-node [ctx nid path]
  (if-let [idx (first path)]
    (let [node (get-node ctx nid)]
      (recur ctx (get-in node [:children idx])
             (next path)))
    nid))

(>defn aast->edn [ctx nid]
  [CtxSchema NodeIdSch '=> :any]
  (let [node (get-node ctx nid)
        expr (::expr node)
        mapseqential (map #(aast->edn ctx %))]
    (cond
      (seq? expr)
      (list* (eduction mapseqential (:children node)))
      (vector? expr)
      (into [] mapseqential (:children node))
      (map? expr)
      (throw (ex-info ""{}))
      :else
      (vary-meta (if(symbol? expr)
                   expr
                   (symbol (pr-str expr)))
                 assoc ::node (get-node ctx nid)))))
