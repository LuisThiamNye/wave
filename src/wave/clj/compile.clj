(ns wave.clj.compile
  (:require
   [wave.compile :as compile]))

(declare desugar-form)

(defn desugar-code [form]
  )

(defn desugar-list [ctx sym args]
  (or (when (symbol? sym)
        (case sym
          quote
          (if (== 1 (count args))
            [:quote {} (first args)]
            (throw (ex-info "quote must have only one argument" {})))
          do
          [:do {} (mapv (partial desugar-form ctx) args)]
          !!_defstatic
          [:cf {} :defstatic (nth args 0) (nth args 1) (nth args 2) (nth args 3)]
          !!_defn
          [:cf {} :defn (nth args 0) (nth args 1) (nth args 2) (nth args 3)]
          !!_compile-outstanding!
          [:cf {} :compile-outstanding!]
          !!_execute!
          [:cf {} :execute (nth args 0)]
          !!_undef
          [:cf {} :undef (nth args 0)]
          (when-let [*m (-> ctx :clj-macros (get (str sym)))]
            (let [{:keys [f sig]} @*m]
             (if sig
               (throw (UnsupportedOperationException.))
               (desugar-form ctx (apply f args)))))))
      [:invoke {}
       (desugar-form ctx sym)
       (mapv (partial desugar-form ctx) args)]))

(defn desugar-form [ctx form]
  (cond
    (list? form)
    (desugar-list ctx (first form) (next form))
    (string? form)
    [:string {} form]
    (symbol? form)
    [:resolve-binding {} (str form)]
    (number? form)
    [:number {} (str form)]
    (keyword? form)
    [:keyword {} form]))

(comment
  (defn add-clj-macros [ctx syms]
    (reduce (fn [ctx sym]
              (assoc-in ctx [:clj-macros (str sym)]
                        (volatile! {:f (fn [& args]
                                         (apply (resolve sym) {} {} args))})))
            ctx
            syms))

  (def *ctx (atom (wave.clj.core/new-evalctx :target-dir (str (System/getProperty "user.dir")"/target/clj"))))

  (swap! *ctx add-clj-macros '[->])

  (def aast
    (desugar-form @*ctx
                  '(do (!!_defstatic "std" {} nil
                                     [:invoke {} [:raw "@import"]
                                      [[:string "std"]]])
                       (!!_compile-outstanding!))))
  (compile/run-cform! *ctx aast)

  #!
  )
