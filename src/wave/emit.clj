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
  {\-    "_"
   \_    "_U_"
   \:    "_COLON_"
   \+    "_PLUS_"
   \>    "_GT_"
   \<    "_LT_"
   \=    "_EQ_"
   \~    "_TILDE_"
   \!    "_BANG_"
   \@    "_ATSYM_"
   \#    "_HASH_"
   \'    "_SINGLEQUOTE_"
   \" "_DOUBLEQUOTE_"
   \%    "_PERCENT_"
   \^    "_CARET_"
   \&    "_AMPERSAND_"
   \*    "_STAR_"
   \|    "_BAR_"
   \{    "_LBRACE_"
   \}    "_RBRACE_"
   \[    "_LBRACK_"
   \]    "_RBRACK_"
   \/    "_SLASH_"
   \\    "_BSLASH_"
   \?    "_QMARK_"})

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

(>defn write-fn [{:keys [writer] :as ctx} {:keys [sym ret-type params body-fn return-fn privacy export?]}]
  [map? [:map [:sym string?] [:ret-type [:maybe keyword?]]] '=> :any]
  (case privacy
    :public (write writer "pub ")
    :private nil)
  (doto writer
    (cond-> export? (write "export "))
    (write "fn ")
    (write (munge sym))
    (write "() ")
    (write (or (name ret-type) "void"))
    (write " {\n"))
  (let [ctx (assoc ctx :indent-level 1)]
    (when body-fn (body-fn ctx))
    (when return-fn
      (write-indent ctx)
      (write writer "return ")
      (return-fn ctx)
      (write writer ";\n")))
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

(defn write-invoke [{:keys [writer] :as ctx} {:keys [obj args]}]
  (write-expr ctx obj)
  (doto writer
    (write "("))
  (write-comma-sep-exprs ctx args)
  (write writer ")"))

(defn write-anon-array [{:keys [writer] :as ctx} {:keys [children]}]
  (write writer ".{")
  (write-comma-sep-exprs ctx children)
  (write writer "}"))

(declare write-expr)

(>defn write-assign [{:keys [writer] :as ctx} {:keys [prefix sym v]}]
  [:any [:map [:sym string?] [:v some?]] '=> :any]
  (doto writer
    (write prefix)
    (write " ")
    (write sym)
    (write " = "))
  (write-expr ctx v)
  (write writer ";\n"))

(defn local-gensym []
  (str/replace (str "tmp" (random-uuid)) "-" "_"))

(defn write-invoke-expr [ctx expr args]
  (match expr
    [:raw sym]
    (write-invoke ctx {:obj [:raw sym] :args args})
    [:varderef sym]
    (write-invoke ctx {:obj [:raw sym] :args args})
    :else
    (write-invoke ctx {:obj expr :args args})
    #_(let [sym (local-gensym)]
      (write-assign ctx {:prefix "const" :sym sym :v expr})
        (write-invoke ctx {:sym sym :args args}))))

(defn write-bind-local [{:keys [writer] :as ctx} {:keys [bindings]} body]
  (letfn [(write' [x] (write writer x))]
    (write-indent ctx)
    (write' "{ // `let`\n")
    (let [ctx (update ctx :indent-level inc)]
      (doseq [[sym expr] bindings]
        (write-assign ctx {:prefix "const"
                           :sym sym
                           :v expr}))
      (write-indent ctx)
      (write-expr ctx body))
    (write-indent ctx)
    (write' "}\n")))

(defn write-number [{:keys [writer]} {:keys [intpart fracpart radix suffix]}]
  (write writer (str intpart (when fracpart (str "." fracpart)))))

(declare write-statement)

(defn write-do [ctx children]
  (doseq [child children]
    (write-statement ctx child)))

(defn write-access-field [{:keys [writer] :as ctx} field obj]
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
    [:string s]
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
    ;; (write-varderef ctx sym)
    [:access-field field obj]
    (write-access-field ctx field obj)
    [:anon-array opts children]
    (write-anon-array ctx (assoc opts :children children))
    [:if c t f]
    (write-if ctx {:condition-fn #(write-expr % c)
                   :tbody-fn #(write-expr % t)
                   :fbody-fn #(write-expr % f)})))

(defn write-statement [{:keys [writer] :as ctx} statement]
  (write-indent ctx)
  (match statement
    [:assign prefix sym v]
    (write-assign ctx {:prefix prefix :sym sym :v v})
    [:bind-local opts body]
    (write-bind-local ctx opts body)
    [:block info children ret]
    (write-block ctx info children ret)
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
        (write-assign ctx {:sym var-name
                           :prefix "const"
                           :v [:invoke "@import" [[:string {:string dep}]]]}))
      (doseq [{:keys [var-id v]} globals]
        (write-assign ctx {:sym var-id
                           :prefix "const"
                           :v v}))
      (write writer "\n")
      (doseq [{:keys [var-id params ret-type body privacy export? return]} functions]
        (write-fn ctx {:sym var-id
                       :export? export?
                       :params params
                       :privacy privacy
                       :ret-type ret-type
                       :body-fn (fn [ctx]
                                  (doseq [s body]
                                    (write-statement ctx s)))
                       :return-fn #(write-expr % return)})))))
