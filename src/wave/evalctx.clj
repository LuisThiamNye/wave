(ns wave.evalctx
  (:require
   [crypticbutter.snoop :refer [>defn]]
   [wave.emit :as emit]
   [clojure.core.match :refer [match]]))

(def FunctionSchema
  [:map
   [:deps
    [:map
     [:vars set?]
     [:globals set?]]]])

(def GlobalVariableSchema
  [:map])

(def EvalCtxSchema
  [:map
   [:lexical
    [:map
     [:parent {:optional true} map?]
     [:parent-fn-sym {:optional true} string?]
     [:bindings
      [:map-of string? :any]]]]
   [:vars
    :any]
   [:globals
    [:map-of :any
     [:or FunctionSchema GlobalVariableSchema]]]])

(defn new-evalctx [& {:keys [target-dir]}]
  {:globals {}
   :functions {}
   :ns-aliases {}
   :vars {}
   :pending []
   :lexical {:bindings {}}
   :target-dir (or target-dir "./target")})

(defn add-require [ctx dep {:keys [alias]}]
  (update ctx :ns-aliases assoc alias dep))

(defn add-fn [ctx sym params body & {:keys [export? return privacy]}]
  (let [var-id sym
        ctx (-> ctx
                (update :functions assoc sym
                        {:params params
                         :privacy (or privacy :public)
                         :export? export?
                         :body body
                         :return return
                         :ret-type :void
                         :var-id var-id}))]
    #_(if (contains? (:functions ctx) var-id))
    ctx))

(>defn add-global [ctx x]
  [EvalCtxSchema :any '=> :any]
  (match x
    [:assign prefix sym (v :guard some?)]
    (let [ctx (update ctx :globals assoc sym
                      {:prefix prefix
                       :v v
                       :var-id sym})]
      (update ctx :vars conj (clojure.lang.MapEntry. sym sym)))))
