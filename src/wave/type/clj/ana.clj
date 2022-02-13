(ns wave.type.clj.ana
  (:require
   [wave.type.clj.type :as cljtype]
   [tech.droit.fset :as fset]
   [wave.type.cmn.ctx :as ctx]
   [wave.type.cmn.type :as type]
   [wave.type.cmn.ana :as ana]
   [wave.type.cmn.aast :as aast]
   [wave.type.clj.aast :as cljaast]
   [com.rpl.specter :as sp]
   [clojure.core.match :refer [match]]
   [crypticbutter.snoop :refer [>defn]]
   [clojure.repl]
   [clojure.reflect :as reflect]))

(defmacro sig
  "Adds additional signature information that must be
  compatible with the inferred signature of the var.
  Should return constraints on the inputs, and on the output"
  [sym ret+params body])

(defn resolve-var [ctx sym]
  (ns-resolve (find-ns (or (some-> (namespace sym) symbol)
                           (:namespace ctx)))
              sym))

(declare analyse)

(>defn analyse-fncall [ctx fexpr args]
  [cljaast/CtxSchema cljaast/EdnNodeSch [:maybe sequential?]
   '=> [:tuple cljaast/CtxSchema cljaast/NodeIdSch]]
  (let [[ctx fexpr'] (analyse ctx fexpr)
        nargs (count args)
        [sig sigerr]
        (match (ctx/lookup-sym ctx fexpr)
          [:lexical info]
          (let [typcsts (:type (get-in ctx (:lexical-id info)))]
            (some (fn [cst]
                    (match cst
                      [:function arityinfo]
                      (if-some [sig (ana/arity->sig arityinfo nargs)]
                        [sig nil]
                        [(ana/arity->sig-fallback arityinfo nargs)
                         [:noarity (str "no arity " nargs)]])
                      :else nil))
                  typcsts))
          [:var _]
          (let [v (resolve-var ctx fexpr)]
            (match (cljtype/java-fn-sig ctx v args)
             [:ok sig]
             [sig nil]
             [:error & err]
             [(ana/arity->sig-fallback (cljtype/java-fn-arityinfo ctx v) nargs)
              (vec err)])))
        [ctx arg's im]
        (reduce
         (fn [[ctx args' im] [i argexpr]]
           (let [[ctx arg'] (analyse ctx argexpr)
                 [ctx arg' im]
                 (if (and sig (not sigerr))
                   (let [[ctx ptype im] (type/instantiate-frees ctx im :join
                                                                (nth (:params sig) i))
                         [ctx results] (type/join-types ctx ptype (cljtype/node-type ctx arg'))
                         ctx (cljaast/annotate-extra ctx arg' results)]
                     [ctx arg' im])
                   [ctx arg' im])]
             [ctx (conj args' arg') im]))
         [ctx [] {}]
         (map-indexed (fn [i x] [i x]) args))
        [ctx ret-type] (type/instantiate-frees ctx im :read (:return sig []))
        [ctx lexpr'] (cljaast/reg-node ctx (list* fexpr args)
                                       {:node-type :invoke
                                        :sig sig
                                        :ret-type ret-type
                                        :errors (if-some [[err _data] sigerr]
                                                  (case err
                                                    :noarity [{:e-type :invalid-arity}])
                                                  [])})
        ctx (cljaast/add-children ctx lexpr' (into [] cat [[fexpr'] arg's]))]
    [ctx lexpr']))

(defn analyse-macro [ctx macro args])

(defn find-macro [ctx sym])

(defn analyse-special-do [ctx exprs]
  (reduce (fn [[ctx :as acc] expr]
            (let [[ctx expr'] (analyse ctx expr)]
              (-> acc
                  (assoc 0 ctx)
                  (update 1 conj expr'))))
          [ctx []]
          exprs))

(defn analyse-special-let [ctx sf args]
  (let [[bind-vec & body] args
        binds (partition 2 bind-vec)
        ctx (ctx/wind-lexical-ctx ctx)
        add-local (fn [ctx sym typ]
                    (let [local {:type typ}
                          id (str (gensym (str "local." sym ".")))]
                      (-> ctx
                          (update :lexical-bindings
                                  assoc sym {:lexical-id [:locals id]})
                          (update :locals assoc id local))))
        [ctx bind-children-nids] (reduce
                      (fn [[ctx :as acc] i]
                        (let [[sym vexpr] (nth binds i)
                              [ctx vexpr'] (analyse ctx vexpr)
                              [ctx sym'] (cljaast/reg-node ctx sym)
                              ctx (add-local ctx sym (cljtype/node-type ctx vexpr'))]
                          (-> acc
                              (assoc 0 ctx)
                              (update 1 assoc (inc (* 2 i)) vexpr')
                              (update 1 assoc (* 2 i) sym'))))
                      [ctx bind-vec]
                      (range (count binds)))
        [ctx binds'] (cljaast/reg-node ctx bind-vec)
        ctx (cljaast/add-children ctx binds' bind-children-nids)
        [ctx body'*] (analyse-special-do ctx body)
        [ctx letsym'] (cljaast/reg-node ctx 'let*)
        [ctx node] (cljaast/reg-node ctx (list* 'let* args)
                                     {:node-type :special-form
                                      :ret-type (if (seq body'*)
                                                  (cljtype/node-type ctx (last body'*))
                                                  [[:special :nil]])})
        ctx (cljaast/add-children ctx node (apply vector letsym' binds' body'*))]
    [(ctx/unwind-lexical-ctx ctx) node]))

(defn analyse-special-fn [ctx sf args]
  (letfn
   [(analyse-arity [ctx [param-vec ?cond-map :as body]]
      (let [?cond-map (when (map? ?cond-map) ?cond-map)
            body (cond-> (next body) ?cond-map next)
            arityn (if (= '& (butlast param-vec))
                     :varargs
                     (count param-vec))
            free-syms (if (= :varargs arityn)
                        (conj (subvec param-vec 0 (- (count param-vec) 2))
                              (peek param-vec))
                        param-vec)
            [ctx params] (reduce (fn [[ctx :as acc] sym]
                                   (let [[ctx id] (type/add-free-binding ctx sym)]
                                     (-> acc
                                         (assoc 0 ctx)
                                         (update 1 conj [[:free-read id]]))))
                                 [ctx []]
                                 free-syms)
            ctx (ctx/wind-lexical-ctx ctx)
            [ctx params'] (reduce (fn [[ctx :as acc] p]
                                    (let [[ctx p'] (cljaast/reg-node ctx p)]
                                      (-> acc
                                          (assoc 0 ctx)
                                          (update 1 conj p'))))
                                  [ctx []]
                                  param-vec)
            [ctx param-vec'] (cljaast/reg-node ctx param-vec)
            ctx (cljaast/add-children ctx param-vec' params')
            [ctx body'] (analyse-special-do ctx body)]
        [(ctx/unwind-lexical-ctx ctx)
         (if ?cond-map
           (apply vector param-vec' ?cond-map body')
           (apply vector param-vec' body'))
         [arityn {:params params
                  :return (if (seq body')
                            (into []
                                  (map (fn [cst]
                                         (if (= :free-join (nth cst 0))
                                           (assoc cst 0 :free-read)
                                           cst)))
                                  (cljtype/node-type ctx (last body')))
                            [[:special :nil]])}]]))
    (-analyse-multi [ctx arities]
      (reduce (fn [[ctx :as acc] arity]
                (let [[ctx arity' [arityn sig]] (analyse-arity ctx arity)]
                  (-> acc
                      (assoc 0 ctx)
                      (update 1 conj arity')
                      (update 2 assoc arityn sig))))
              [ctx [] []]
              arities))]
    (let [?sym (first args)
          ?sym (when (symbol? ?sym) ?sym)
          args2 (cond-> args ?sym next)
          [ctx args2' sigs] (if (vector? (first args2))
                              (update (analyse-arity ctx args2)
                                      2 (fn [[arityn sig]] {arityn sig}))
                              (-analyse-multi ctx args2))
          [ctx node] (cljaast/reg-node ctx (list* 'fn* args)
                                       {:node-type :special-form
                                        :ret-type [[:function sigs]]})
          [ctx ?sym'] (if ?sym (cljaast/reg-node ctx ?sym) [ctx nil])
          [ctx fnexpr] (cljaast/reg-node ctx 'fn*)
          ctx (cljaast/add-children
               ctx node (if ?sym
                          (apply vector fnexpr ?sym' args2')
                          (apply vector fnexpr args2')))]
      [ctx node])))

(defn analyse-special [ctx sf args]
  (case sf
    :do (let [[ctx nodes] (analyse-special-do ctx args)
              [ctx node] (cljaast/reg-node ctx (list* 'do args)
                                           {:node-type :special-form
                                            :ret-type (cljtype/node-type ctx (last nodes))
                                            :children nodes})]
          [ctx node])
    :let* (analyse-special-let ctx sf args)
    :fn* (analyse-special-fn ctx sf args)))

(defn find-special [sym]
  (case (str sym)
    "do" :do
    "let*" :let*
    "fn*" :fn*
    nil))

(defn analyse-symbol [ctx sym]
  (if-let [[binding-type info] (ctx/lookup-sym ctx sym)]
    (cljaast/reg-node ctx sym
                      {:node-type (case binding-type
                                    :lexical :lexical-usage
                                    :var :var-usage)
                       :lexical-id (:lexical-id info)
                       :ctx (dissoc ctx :global-bindings :annotations)
                       :errors []})
    (cljaast/reg-node ctx sym {:node-type :unresolved-symbol})))

(>defn analyse [ctx expr]
  [cljaast/CtxSchema cljaast/EdnNodeSch
   '=> [:tuple cljaast/CtxSchema cljaast/NodeIdSch]]
  (cond
    (or (instance? clojure.lang.Cons expr) (list? expr))
    (case (count expr)
      0 [ctx ()]
      (let [[cmd & args] expr]
        (or (when (symbol? cmd)
              (or (when-let [m (find-macro ctx cmd)]
                    (analyse-macro ctx m args))
                  (when-let [sf (find-special cmd)]
                    (analyse-special ctx sf args))))
            (analyse-fncall ctx cmd args))))
    (symbol? expr)
    (analyse-symbol ctx expr)
    :else
    (cljaast/reg-node ctx expr {:node-type :simple})))

(defn infer [ctx expr])

;; (defn infer-opaque [ctx sym]
;;   (infer ctx (read-string (clojure.repl/source-fn sym))))

(>defn get-errors [ctx]
  [map? '=> :any]
  (into []
        (filter (fn [a]
                  (seq (:errors a))))
        (vals (:nodes ctx))))

(defn fresh-ctx []
  (with-meta
    {:namespace 'user
     :annotations []
     :nodes {}
     :locals {}
     :lexical-bindings {}
     :global-bindings {"clojure.core"
                       (into {} (map (fn [[sym _thevar]] [(name sym) {}]))
                             (ns-interns (find-ns 'clojure.core)))}}
    {`type/type-errors cljtype/type-errors}))

(def dctx (fresh-ctx))

(comment
  (def testform ;;
    #_'(let* [x (fn* [a] a)]
       (clojure.core/subs 5 x))
    #_'(clojure.core/subs "" 3 4 4)
    #_'(let* [f (fn* [] "")] (f 1))
    '(let* [f (fn* [a] a)] (f 2)))

  (try
    (let [[ctx nid] (analyse dctx testform)]
      (vary-meta (cljaast/aast->edn ctx nid) assoc :ctx (dissoc ctx :global-bindings :annotations))
      ;; (vary-meta node assoc :ctx (dissoc ctx :global-bindings :annotations))
      ;; (get-errors x)
      ;; (map :errors (:annotations (first x)))
      ;;
      )
    (catch Exception _))
  (sp/select [0] '(1 2))

  (require 'crypticbutter.snoop.config :reload)
  (require 'crypticbutter.snoop :reload)
  (tap> [1 23 4])
  (vlaaad.reveal/ui)

  #'taoensso.encore/catching

  (analyse dctx 'xy)
  (find-ns 'clojure.core)

  (meta #'subs)
  (read-string (clojure.repl/source-fn 'str))
  (find-sig {} 'assoc [1 2 2 2 2])

  (filter #(= 'assoc (:name %)) (:members (clojure.reflect/reflect clojure.lang.RT)))
  (filter #(= 'get (:name %)) (:members (clojure.reflect/reflect clojure.lang.RT)))
  (map :name (:members (clojure.reflect/reflect (class subs))))
  (filter #(= 'invokeStatic (:name %)) (:members (clojure.reflect/reflect (class subs))))
  (.getRequiredArity (deref (resolve 'str)))
  (meta (second (read-string (clojure.repl/source-fn 'subs))))
  (type (:tag (meta (ffirst (:arglists (meta #'subs))))))

  (class assoc)

  #!
  )
