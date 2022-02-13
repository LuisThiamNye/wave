(ns wave.emit
  (:refer-clojure :exclude [munge])
  (:require
   [clojure.set :as cset]
   [babashka.fs :as fs]
   [crypticbutter.snoop :as snoop :refer [>defn]]
   [clojure.core.match :refer [match]]
   [clojure.java.io :as io]
   [clojure.string :as str]))

(def MUNGE_MAP
  {\- "_"
   \_ "_U_"
   \: "_COLON_"
   \+ "_PLUS_"
   \> "_GT_"
   \< "_LT_"
   \= "_EQ_"
   \~ "_TILDE_"
   \! "_BANG_"
   \@ "_ATSYM_"
   \# "_HASH_"
   \' "_SINGLEQUOTE_"
   \" "_DOUBLEQUOTE_"
   \% "_PERCENT_"
   \^ "_CARET_"
   \& "_AMPERSAND_"
   \* "_STAR_"
   \| "_BAR_"
   \{ "_LBRACE_"
   \} "_RBRACE_"
   \[ "_LBRACK_"
   \] "_RBRACK_"
   \/ "_SLASH_"
   \\ "_BSLASH_"
   \? "_QMARK_"})

;; (def DEMUNGE_MAP (cset/map-invert MUNGE_MAP))

(defn munge [sym]
  (let [sb (StringBuilder.)]
    (doseq [c (.toCharArray sym)]
      (let [sub (.valAt MUNGE_MAP c)]
        (if (nil? sub)
          (.append sb sub)
          (.append sb c))))
    (.toString sb))
  (apply str (replace MUNGE_MAP sym)))

(>defn write [^java.io.Writer writer content]
  [:any string? '=> :any]
  (try (.write writer content)
       (catch Exception e
         (println "\nproblem writing")
         (println e))))

(defn write-indent [{:keys [indent-level writer]}]
  (dotimes [_ indent-level]
    (write writer "  ")))

(defn write-def [{:keys [writer] :as ctx} {:keys [sym value-fn]}]
  (doto writer
    (write "const ")
    (write (munge sym))
    (write " "))
  (value-fn ctx)
  (write writer ";\n"))

(>defn write-fn [{:keys [writer] :as ctx} {:keys [sym ret-type params body-fn privacy export?]}]
  [map? [:map [:sym string?] [:ret-type [:maybe [:or string? keyword?]]]] '=> :any]
  (case privacy
    :public (write writer "pub ")
    (:private nil) nil)
  (doto writer
    (cond-> export? (write "export "))
    (write "fn ")
    (write (munge sym))
    (write "() ")
    (write (or (name ret-type) "void"))
    (write " {\n"))
  (let [ctx (assoc ctx :indent-level 1)]
    (when body-fn (body-fn ctx)))
  (write writer "}\n"))

(defn write-if [{:keys [writer] :as ctx} {:keys [condition-fn tbody-fn fbody-fn]}]
  (when tbody-fn
    (write writer "if (")
    (condition-fn (assoc ctx :indent-level 0))
    (write writer ") {\n")
    (tbody-fn (assoc ctx :indent-level 1))
    (write writer "}")
    (if fbody-fn
      (do (write writer " else {\n")
          (fbody-fn (assoc ctx :indent-level 1))
          (write writer "}\n"))
      (write writer "\n"))))

(declare write-expr)

(defn write-return [{:keys [writer] :as ctx} expr]
  (write writer "return ")
  (write-expr ctx expr)
  (write writer ";\n"))

(defn write-comma-sep-exprs [{:keys [writer] :as ctx} exprs]
  (let [nargs (count exprs)]
    (when (pos? nargs)
      (loop [i 0]
        (let [arg (nth exprs i)]
          (write-expr ctx arg)
          (let [i2 (inc i)]
            (when (< i2 nargs)
              (write writer ", ")
              (recur i2))))))))

(defn write-array [{:keys [writer] :as ctx} {:keys [children]}]
  (write writer ".{")
  (write-comma-sep-exprs ctx children)
  (write writer "}"))

(declare write-expr)

(>defn write-bind [{:keys [writer] :as ctx} {:keys [prefix sym v]}]
  [:any [:map [:sym string?] [:v some?]] '=> :any]
  (write-indent ctx)
  (doto writer
    (write prefix)
    (write " ")
    (write sym)
    (write " = "))
  (write-expr ctx v)
  (write writer ";\n"))

(defn local-gensym []
  (str/replace (str "tmp" (random-uuid)) "-" "_"))

(defn write-invoke-expr [{:keys [writer] :as ctx} expr args]
  (write-expr ctx expr)
  (doto writer
    (write "("))
  (write-comma-sep-exprs ctx args)
  (write writer ")"))

(declare write-statement)

(defn write-bind-local [{:keys [writer] :as ctx} {:keys [bindings]} body]
  (letfn [(write' [x] (write writer x))]
    (write' "{ // `let`\n")
    (let [ctx (update ctx :indent-level inc)]
      (doseq [[prefix sym expr] bindings]
        (write-bind ctx {:prefix (name prefix)
                         :sym sym
                         :v expr}))
      (doseq [statement body]
        (write-statement ctx statement)))
    (write-indent ctx)
    (write' "}\n")))

(defn write-number [{:keys [writer]} {:keys [intpart fracpart radix suffix]}]
  (write writer (str intpart (when fracpart (str "." fracpart)))))

(defn write-do [ctx children]
  (doseq [child children]
    (write-statement ctx child)))

(defn write-access [{:keys [writer] :as ctx} field obj]
  (write-expr ctx obj)
  (write writer ".")
  (write writer field))

#_(defn write-varderef [{:keys [writer] :as ctx} sym]
    (write-expr ctx obj)
    (write writer ".")
    (write writer field))

(defn write-block [{:keys [writer] :as ctx} {:keys [id comment]} children ret]
  (let [id (str (gensym))]
    (when id
      (doto writer
        (write id)
        (write ": ")))
    (write writer "{")
    (when comment
      (write writer " // ")
      (write writer comment))
    (write writer "\n")
    (let [ctx (update ctx :indent-level inc)]
      (doseq [c children]
        (write-statement ctx c))
      (when ret
        (write-indent ctx)
        (write writer "break :")
        (write writer id)
        (write writer " ")
        (write-expr ctx ret)
        (write writer ";\n")))
    (write-indent ctx)
    (write writer "}")))

(defn write-expr [{:keys [writer] :as ctx} expr]
  (match expr
    [:raw s]
    (write writer s)
    [:string {} s]
    (doto writer
      (write "\"")
      (write s)
      (write "\""))
    [:invoke {} expr' args]
    (write-invoke-expr ctx expr' args)
    [:do children]
    (write-do ctx children)
    [:number info]
    (write-number ctx info)
    [:block info children ret]
    (write-block ctx info children ret)
    [:local sym]
    (write writer sym)
    [:global {:var-id sym}]
    (write writer sym)
    [:varderef sym]
    (write writer sym)
    [:resolve sym]
    (write writer sym)
    ;; (write-varderef ctx sym)
    [:access field obj]
    (write-access ctx field obj)
    [:array opts children]
    (write-array ctx (assoc opts :children children))
    [:if c t f]
    (write-if ctx {:condition-fn #(write-expr % c)
                   :tbody-fn #(write-expr % t)
                   :fbody-fn #(write-expr % f)})))

(defn write-statement [{:keys [writer] :as ctx} statement]
  (write-indent ctx)
  (match statement
    [:assign prefix sym v]
    (write-bind ctx {:prefix prefix :sym sym :v v})
    [:bind-local opts body]
    (write-bind-local ctx opts body)
    [:block info children ret]
    (write-block ctx info children ret)
    [:return {} expr]
    (write-return ctx expr)
    :else (do (write-expr ctx statement)
              (write writer ";\n"))))

(def ^:const target-dir-default (str (fs/path (System/getProperty "user.dir") "target")))
(def ^:const zig-src-folder "zig-src")
(def ^:const zig-out-folder "zig-out")

(defn prepare-dirs [target-dir]
  (let [zig-dir (fs/path target-dir zig-src-folder)
        zig-out-dir (fs/path target-dir zig-out-folder)]
    (when-not (fs/exists? zig-dir)
      (fs/create-dirs zig-dir))
    (when-not (fs/exists? zig-out-dir)
      (fs/create-dirs zig-out-dir))))

(defn emit-unit [{:keys [target-dir] :as ctx} unit-id {:keys [functions globals global-imports]}]
  (prepare-dirs target-dir)
  (with-open [writer (io/writer (str (fs/path target-dir zig-src-folder (str unit-id ".zig"))))]
    (let [ctx {:writer writer :indent-level 0}]
      (doseq [{:keys [var-name dep]} global-imports]
        (write-bind ctx {:sym var-name
                         :prefix "const"
                         :v [:invoke "@import" [[:string {:string dep}]]]}))
      (doseq [{:keys [var-id v]} globals]
        (write-bind ctx {:sym var-id
                         :prefix "const"
                         :v v}))
      (write writer "\n")
      (doseq [{:keys [var-id params ret-type body privacy export?]} functions]
        (write-fn ctx {:sym var-id
                       :export? export?
                       :params params
                       :privacy privacy
                       :ret-type ret-type
                       :body-fn (fn [ctx]
                                  (doseq [s body]
                                    (write-statement ctx s)))})))))

(defn emit-all [{:keys [root-dir files]}]
  (prepare-dirs root-dir)
  (doseq [[filename fdata] files]
    (with-open [writer (io/writer (str (fs/path root-dir zig-src-folder (str filename ".zig"))))]
      (let [ctx {:writer writer :indent-level 0}]
        (doseq [node (:nodes fdata)]
          (match node
            [:cvalue {} mut sym vexpr]
            (write-bind ctx {:sym sym
                             :prefix (name mut)
                             :v vexpr})
            [:cfn opts sym params ret-type body]
            (write-fn ctx {:sym sym
                           :export? (:export? opts)
                           :params params
                           :privacy (:visibility opts)
                           :ret-type ret-type
                           :body-fn (fn [ctx]
                                      (doseq [s body]
                                        (write-statement ctx s)))}))
          (write writer "\n"))))))
