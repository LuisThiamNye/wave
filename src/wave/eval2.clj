(ns wave.eval2
  (:require
   [crypticbutter.snoop :refer [>defn]]
   [wave.evalctx :as evalctx]
   [clojure.core.match :refer [match]]
   [clojure.zip :as zip]
   [clojure.string :as str]))

(defmulti eval-special-form (fn [_ctx sym _args]
                      sym))

(defn restore-lexical-context [ctx]
  (assoc ctx :lexical (-> ctx :lexical :parent)))

(defn update-lexical-context [{:keys [lexical]:as ctx} f]
  (assoc ctx :lexical (assoc (f lexical)
                             :parent lexical)))

(declare compile-exprs)
(declare eval-form)
(defn compile-expr [ctx expr]
  (eval-form ctx expr)
  #_(match expr
    [:do _ body]
    (let [[ctx body] (compile-exprs ctx body)]
      {:result [:do body]
       :ctx ctx})
    [:string s]
    {:result [:string s]
     :ctx ctx}
    [:vector _ children]
    (let [[ctx children] (compile-exprs ctx children)]
      {:result [:anon-array {} children]
       :ctx ctx})
    [:list _ children]
    (eval-special-form ctx cmd args)))

(>defn compile-exprs [ctx exprs]
  [evalctx/EvalCtxSchema [:maybe [:sequential vector?]] '=> [:tuple some? :any]]
  (reduce
   (fn [[ctx :as acc] x]
     (let [{:keys [result ctx]} (compile-expr ctx x)]
       (-> acc
           (assoc 0 ctx)
           (update 1 conj result))))
   [ctx []]
   exprs))

(defn resolve-symbol [ctx sym]
  (let [symname (match sym [:symbol _ symname] symname)
        segments (cond-> (str/split symname #"/")
                   (str/ends-with? symname "/")
                   (conj "/"))]
    (if (== 1 (count segments))
      symname
      (str (get-in ctx [:ns-aliases (nth segments 0)])
           "."
           (str/join "." (next segments))))))

(defmethod eval-special-form :default [ctx sym args]
  (let [sym (resolve-symbol ctx sym)
        [ctx args] (compile-exprs ctx args)]
    {:result [:invoke sym args]
     :ctx ctx}))

(defn eval-expr [ctx expr]
  (match expr
    [:quote _ [thing]]
    {:result thing
     :ctx ctx}))

(>defn eval-exprs [ctx exprs]
  [evalctx/EvalCtxSchema sequential? '=> :any]
  (reduce
   (fn [[ctx :as acc] x]
     (let [{:keys [result ctx]} (eval-expr ctx x)]
       (-> acc
           (assoc 0 ctx)
           (update 1 conj result))))
   [ctx []]
   exprs))

(defn ensure-nargs [n args]
  (let [c (count args)
        n (if (set? n) n (hash-set n))]
    (when-not (or (when (contains? n :vararg)
                    (<= (apply max (disj n :vararg)) c))
                  (n c))
      (throw (Exception. "wrong number of args")))))

(defmethod eval-special-form "-" [ctx _ args]
  (ensure-nargs #{1 :vararg} args)
  (let [[ctx args] (eval-exprs ctx args)]
    {:result [:binaryop {:op :subtract} [args]]
     :ctx ctx}))

(defmethod eval-special-form "+" [ctx _ args]
  (ensure-nargs #{1 :vararg} args)
  (let [[ctx args] (eval-exprs ctx args)]
    {:result (case (count args)
                   0 [:number {:intpart "0"}]
                   1 (nth args 0)
                   [:binaryop {:op :add} [args]])
     :ctx ctx}))

(defmethod eval-special-form "*" [ctx _ args]
  (let [[ctx args] (eval-exprs ctx args)]
    {:result (case (count args)
                   0 [:number {:intpart "1"}]
                   1 (nth args 0)
                   [:binaryop {:op :multiply} [args]])
     :ctx ctx}))

(defmethod eval-special-form "/" [ctx _ args]
  (ensure-nargs #{1 :vararg} args)
  (let [[ctx args] (eval-exprs ctx args)]
    {:result (if (== 1 (count args))
                   [:binaryop {:op :divide} [[:number {:intpart "1"}] (nth args 0)]]
                   (reduce (fn [acc x]
                             [:binaryop {:op :divide} [acc x]])
                           args))
     :ctx ctx}))

(defmethod eval-special-form "import" [ctx _ args]
  (ensure-nargs 1 args)
  {:result [:invoke [:raw "@import"] [(nth args 0)]]
   :ctx ctx})

(defmethod eval-special-form "require" [ctx _ args]
  (ensure-nargs #{1 :vararg} args)
  (let [[ctx args] (eval-exprs ctx args)]
    {:ctx (reduce (fn [ctx decl]
                    (match decl
                      [:vector _ [[:string dep]
                                  [:keyword {:content "as"}]
                                  [:symbol {} alias-sym]]]
                      (evalctx/add-require ctx dep {:alias alias-sym})))
                  ctx
                  args)
     :result [:nil]}))

(defmethod eval-special-form "def" [ctx _ args]
  (ensure-nargs #{2} args)
  (let [[sym v] args
        sym (match sym
              [:symbol _ sym']
              sym')
        {:keys [result ctx]} (compile-expr ctx v)]
    {:result [:nil]
     :ctx (evalctx/add-global
           ctx
           [:assign :const sym result])}))

(defn ensure-as-expr [ctx statement]
  #_[:block {:id (str (gensym))} [statement]]
  statement)

(defmethod eval-special-form "fn" [ctx _ args]
  (ensure-nargs #{2 3 :vararg} args)
  (let [[sym params body] (match [args]
                              [([[:symbol _ sym] params & body] :seq)]
                              [sym params body]
                              [([params & body] :seq)]
                              [(str (gensym)) params body])
        sym (if-let [p (-> ctx :lexical :parent-fn-sym)]
              (str p "__fn_" sym)
              sym)
        [ctx body] (compile-exprs (update-lexical-context
                                   ctx (fn [l]
                                         (assoc l :parent-fn-sym sym)))
                                  body)]
    {:result [:varderef sym]
     :ctx (evalctx/add-fn
           ctx sym params (butlast body)
           :return (ensure-as-expr ctx (last body)))}))

(defmethod eval-special-form "defn" [ctx _ args]
  (ensure-nargs #{2 3 :vararg} args)
  (let [res (eval-special-form ctx "fn" args)]
    {:result [:nil]
     :ctx (:ctx res)}))

(defmethod eval-special-form "let" [ctx _ body]
  (ensure-nargs #{2 :vararg} body)
  (match (first body)
    [:vector _ vec-items]
    (let [body (next body)
          pairs (partition 2 vec-items)
          {:keys [bindings ctx]}
          (reduce
           (fn [{:keys [ctx] :as acc} [symnode v]]
             (match symnode
               [:symbol _info sym]
               (let [{:keys [result ctx]} (compile-expr ctx v)]
                 (-> acc
                     (update :bindings conj
                             (clojure.lang.MapEntry. sym result))
                     (assoc :ctx
                            (update-in ctx [:lexical :bindings]
                                       conj
                                       (clojure.lang.MapEntry. sym sym)))))))
           {:bindings {}
            :ctx ctx}
           pairs)
          [ctx body] (compile-exprs ctx body)]
      {:result [:block {:comment "let"}
                (into (mapv (fn [[sym result]]
                              [:assign "const" sym result])
                            bindings)
                      (butlast body))
                (peek body)]
       :ctx (restore-lexical-context ctx)})))

(defmethod eval-special-form "if" [ctx _ body]
  (ensure-nargs 2 body)
  (let [[ctx [c t f]] (compile-exprs ctx body)]
    {:result [:if c t f]
     :ctx ctx}))

(declare eval-form)

(>defn eval-list-form [ctx _info [cmdform & args]]
  [evalctx/EvalCtxSchema :any sequential?
   '=> [:map [:result some?] [:ctx some?]]]
  (let [{:keys [ctx result]} (eval-form ctx cmdform)]
    (match result
      [:special-form sym]
      (eval-special-form ctx sym args)
      :else
      (let [[ctx args] (compile-exprs ctx args)]
        {:result [:invoke result args]
         :ctx ctx}))))

(>defn eval-symbol [ctx _info sym]
  [evalctx/EvalCtxSchema :any string? '=> [:map [:result some?] [:ctx some?]]]
  (let [[sym & fields] (str/split sym #"\.")
        obj (if (some #{sym} (keys (methods eval-special-form)))
            [:special-form sym]
            (if-let [local (get (-> ctx :lexical :bindings) sym)]
              [:local local]
              (if-let [g (get (:globals ctx) sym)]
                [:global g]
                (if-let [v (get (:vars ctx) sym)]
                  [:varderef v]
                  (throw (ex-info (str "Use of unbound symbol " sym) ctx))))))]
    {:ctx ctx
     :result (reduce (fn [acc field]
                       [:access-field field acc])
                     obj
                     fields)}))

(>defn eval-form [ctx form]
  [evalctx/EvalCtxSchema [:and vector? [:cat keyword? [:+ :any]]]
   '=> [:map [:result some?] [:ctx some?]]]
  (match form
    [:list info children]
    (eval-list-form ctx info children)
    [:symbol info sym]
    (eval-symbol ctx info sym)
    [:string s]
    {:result [:string s]
     :ctx ctx}
    [:number n]
    {:result [:number n]
     :ctx ctx}
    [:vector _ children]
    (let [[ctx children] (compile-exprs ctx children)]
      {:result [:anon-array {} children]
       :ctx ctx})))

(defn eval-forms [ctx forms]
  (reduce (fn [ctx c]
            (:ctx (eval-form ctx c)))
          ctx
          forms))

(comment

#!
  )
