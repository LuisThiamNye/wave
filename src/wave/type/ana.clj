(ns wave.type.ana
  (:require
   [tech.droit.fset :as fset]
   [cascade.hike :as hike]
   [clojure.core.match :refer [match]]))

(defn type-or [& alts]
  (into [:or] alts))

(defn type-java [t]
  [:prim t])

(def java-int-types ["java.lang.Integer" "java.lang.Long"])
(def java-frac-types ["java.lang.Float" "java.lang.Double"])

(defn fractional-java-number-type? [t]
  (boolean ((set java-frac-types) (nth t 1))))

(defn widest-number [ns]
  (reduce (fn [widest n]
            (if (fractional-java-number-type? n)
              (type-java "java.lang.Double")
              (if (fractional-java-number-type? widest)
                (type-java "java.lang.Double")
                (type-java "java.lang.Long"))))
          (first ns)
          (next ns)))

(defn bigint? [t]
  (= t [:prim "clojure.lang.BigInt"]))

(defn ret-type-check [expr]
  (cond
    (symbol? expr) :symbol
    (or (instance? clojure.lang.Cons expr)
        (list? expr))
    (if (seq expr)
      (first expr)
      :LIST)
    :else :primitive))

(defmulti ret-type
  (fn [expr]
    (ret-type-check expr)))

(defmethod ret-type :default [_]
  :any)

(defmethod ret-type :primitive [x]
  (cond
    (string? x) :string
    (instance? java.util.regex.Pattern x) (type-java "java.util.regex.Pattern")
    (int? x) (type-java "java.lang.Long")
    (double? x) (type-java "java.lang.Double")
    (bigint? x) (type-java "clojure.lang.BigInt")))

(defmethod ret-type :symbol [sym]
  (or (get-in (ctx ) [:syms (str sym)])
      (throw (ex-info "variable not found" {}))))

(defmethod ret-type `println [_ctx _]
  :nil)

(defmethod ret-type `get [ctx expr]
  (let [k->v (:t/k->v (meta (nth expr 1)))
        vtype (k->v (ret-type ctx (nth expr 2)))]
    (if-some [ftype (nth expr 3 nil)]
      (type-or vtype ftype)
      vtype)))

(defmethod ret-type `assoc [ctx [_ m & kvs]]
  (ret-type ctx m))

(defn ret-type-add [ctx [_ & ns]]
  (cond
    (= 1 (count ns))
    (ret-type ctx (first ns))
    (= 0 (count ns))
    (type-java "java.lang.Long")
    :else
    (let [widest (widest-number (map (partial ret-type ctx) ns))]
      (cond
        (bigint? widest)
        (type-java "clojure.lang.BigInt")
        (fractional-java-number-type? widest)
        (type-java "java.lang.Double")
        :else (type-java "type.lang.Long")))))

(defmethod ret-type `+ [ctx expr]
  (ret-type-add ctx expr))

(defmethod ret-type `- [ctx expr]
  (ret-type-add ctx expr))

(defmethod ret-type `* [ctx expr]
  (ret-type-add ctx expr))

(defmethod ret-type `/ [_ctx expr]
  (if (some fractional-java-number-type? expr)
    (type-java "java.lang.Double")
    (type-java "clojure.lang.Ratio")))

(defmethod ret-type `identity [ctx expr]
  (ret-type ctx (nth expr 1)))

(defn annotate-expr
  "TODO
  Takes a code expression and returns static type analysis data
  - Errors
  - List of constraints for inference
  ctx includes local and global data."
  ([expr] (annotate-expr (:t/ctx (meta expr)) expr))
  ([ctx expr]))

;; Static analysis process:
;; 'executes' through static code to build up a context of constraints for underspecified varibles
;; variables could be fully specified in which case errors would be output if the usage conflicts
;; conflicting use of literals also returns errors

(comment
  (type (/ 3))
  (ret-type {:syms {(str `x) :x}}
            `(identity x))
  (ret-type `(let [x 5]
               x))
  (ns-unmap *ns* 'ret-type)

  (defn _ []
    (let [x
          (fn [y]
            (+ 3 y))]
      (set! x 1)
      (str x)))
  ;; constraints
  '{x1 [lambda..]
    x2 [mutable]
    x3 [stringifyable]
    y1 [free]
    y2 [number]}

  ;; uses of variables adds constriaints to them. constraints are attached to parts of the AST where they are introduced
  ;; variables are stored in a global context with unique ids different to the binding symbols
  ;; there may be multiple variables per binding (eg to represent a mutable value in different points in time)
  ;; different parts of code may be analysed with different local contexts with information to resolve bindings to the underlying variables
  (let [a "" ;; constrains a variable to type string
        x
        (fn [y]
          (set! a y) ;; side effect becomes a part of the function signature
          (+ 3 y))]
    (x 2) ;; changes local context, so subsequent `a` refers to variable of number type
    (str a))

  #!
  )
