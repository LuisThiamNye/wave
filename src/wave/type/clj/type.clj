(ns wave.type.clj.type
  (:require
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

(defn java-class-name [^Class c]
  (.getName c))

(def any-type [])

(defn java-class->type [c]
  [[:type (cond
            (class? c) (.getName ^Class c)
            (string? c) c
            :else (throw (ex-info (str "Wrong arg type " c " of type " (type c))
                                  {:arg c})))]])

(>defn node-type [ctx nid]
  [cljaast/CtxSchema cljaast/NodeIdSch '=> type/Schema]
  (let [node (cljaast/get-node ctx nid)]
    (case (:node-type node)
      :unresolved-symbol (java-class->type "java.lang.Object")
      :simple (java-class->type (type (::cljaast/expr node)))
      :invoke (:ret-type node)
      :lexical-usage (if (= :frees (first (:lexical-id node)))
                       [[:free-join (nth (:lexical-id node) 1)]]
                       (:type (aast/node->lexical node)))
      :special-form (:ret-type node)
      (throw (ex-info (str "no type data for " node) {})))))

(defn class-ancestor-of? [parent-tstr tstr]
  (contains? (into #{} (map #(java-class-name %))
                   (ancestors (resolve (symbol tstr))))
             parent-tstr))

(>defn type-errors [_ctx limit-type typex]
  [:any type/Schema type/Schema '=> [:vector map?]]
  (or
   (some->
    (some
     (fn [cst]
       (match cst
         [:type tstr]
         (some (fn [cst2]
                 (match cst2
                   [:type tstr2]
                   (when-not (or (= tstr tstr2) (class-ancestor-of? tstr tstr2))
                     {:msg (str "Type " tstr " does not satisfy " tstr2)})
                   [:function _arityinfo]
                   (when-not (contains? (into #{} (map #(java-class-name %))
                                              #{java.io.Serializable
                                                clojure.lang.Fn
                                                clojure.lang.AFunction
                                                clojure.lang.IMeta
                                                java.util.Comparator
                                                clojure.lang.IObj
                                                clojure.lang.AFn
                                                java.lang.Object
                                                java.util.concurrent.Callable
                                                java.lang.Runnable
                                                clojure.lang.IFn})
                                        tstr)
                     {:msg (str "Type 'function' does not satisfy " tstr)})
                   :else nil))
               typex)
         [:function _arityinfo]
         (some (fn [cst2]
                 (match cst2
                   [:type tstr]
                   (when-not (contains? (into #{} (map #(.getName %))
                                              (ancestors (resolve (symbol tstr))))
                                        "java.lang.Runnable")
                     {:msg (str "Type " tstr " is not a runnable function")})
                   :else nil))
               typex)
         :else nil))
     limit-type)
    vector)
   []))

(defn raw-java-fn-sigs-ecn [thevar]
  #_(eduction
     (filter #(= 'invokeStatic (:name %)))
     cmembers)
  (eduction
   (map (fn [al]
          {:parameter-types
           (mapv (fn [sym]
                   (or (some-> (-> sym meta :tag)
                               ;; 'Object -> 'java.lang.Object
                               (-> resolve java-class-name symbol))
                       'java.lang.Object))
                 al)
           :return-type 'java.lang.Object}))
   (:arglists (meta thevar))))

(>defn java-fn-arityinfo [ctx thevar]
  [:any :any '=> [:map-of ana/NArgsSch ana/ArityInfoSch]]
  (let [f (deref thevar)
        cmembers (:members (reflect/reflect (class f)))
        reqarity (when (some #(= (:name %) 'getRequiredArity) cmembers) (.getRequiredArity f))
        arityinfo
        (into {}
              (map (fn [{:keys [parameter-types return-type] :as invks}]
                     (let [nparams (count parameter-types)]
                       (if (and reqarity (= nparams (inc reqarity))) ;; varags
                         (clojure.lang.MapEntry.
                          :varargs
                          {:params (mapv (fn [t] (java-class->type (name t))) (pop parameter-types))
                           :return (java-class->type (name return-type))})
                         (clojure.lang.MapEntry.
                          nparams
                          {:params (mapv (fn [t] (java-class->type (name t))) parameter-types)
                           :return (java-class->type (name return-type))})))))
              (raw-java-fn-sigs-ecn thevar))]
    arityinfo))

(>defn java-fn-sig [ctx thevar args]
  [:any :any :any '=> [:and vector? [:cat [:enum :error :ok] [:+ :any]]]]
  (let [f (deref thevar)
        nargs (count args)
        cmembers (:members (reflect/reflect (class f)))
        reqarity (when (some #(= (:name %) 'getRequiredArity) cmembers) (.getRequiredArity f))]
    (or (some->>
         (some
          (fn [{:keys [parameter-types] :as invks}]
            (let [nparams (count parameter-types)]
              (if (and reqarity (< reqarity nargs)
                       (= nparams (inc reqarity))) ;; varags
                {:params (into (mapv (fn [t] (java-class->type (name t))) (pop parameter-types))
                               (repeat (- nargs reqarity) (java-class->type "java.lang.Object")))
                 :what "vararg java fn"
                 :return (java-class->type (name (:return-type invks)))}
                (when (= nparams nargs)
                  {:params (mapv (fn [t] (java-class->type (str t))) parameter-types)
                   :what "arity java fn"
                   :return (java-class->type (name (:return-type invks)))}))))
          (raw-java-fn-sigs-ecn thevar))
         (vector :ok))
        [:error :noarity {}])))

(comment
  (java-class->type String)
  (type java.lang.String)

  #!
  )
