(ns wave.eval
  (:require
   [crypticbutter.snoop :refer [>defn]]
   [wave.evalctx :as evalctx]
   [clojure.core.match :refer [match]]
   [clojure.zip :as zip]
   [clojure.string :as str]))

(defmulti eval-form (fn [_ctx sym _args]
                      sym))

(declare compile-exprs)
(defn compile-expr [ctx expr]
  {:pre [(indexed? expr) (keyword? (nth expr 0))]}
  (match expr
    [:do _ body]
    (let [[ctx body] (compile-exprs ctx body)]
      {:expression [:do body]
       :ctx ctx})
    [:string s]
    {:expression [:string s]
     :ctx ctx}
    [:vector _ children]
    (let [[ctx children] (compile-exprs ctx children)]
      {:expression [:anon-array {} children]
       :ctx ctx})
    [:list _ [cmd & args]]
    (eval-form ctx cmd args)))

(>defn compile-exprs [ctx exprs]
  [[:map [:ns-aliases map?]] sequential? '=> :any]
  (reduce
   (fn [[ctx :as acc] x]
     (let [{:keys [expression ctx]} (compile-expr ctx x)]
       (-> acc
           (assoc 0 ctx)
           (update 1 conj expression))))
   [ctx []]
   exprs))

(defn resolve-symbol [ctx sym]
  (let [symname (match sym [:symbol symname] (:symname symname))
        segments (cond-> (str/split symname #"/")
                   (str/ends-with? symname "/")
                   (conj "/"))]
    (if (== 1 (count segments))
      symname
      (str (get-in ctx [:ns-aliases (nth segments 0)])
           "."
           (str/join "." (next segments))))))

(defmethod eval-form :default [ctx sym args]
  (let [sym (resolve-symbol ctx sym)
        [ctx args] (compile-exprs ctx args)]
    {:expression [:invoke sym args]
     :ctx ctx}))

(defn eval-expr [ctx expr]
  (match expr
    [:quote _ [thing]]
    {:result thing
     :ctx ctx}))

(>defn eval-exprs [ctx exprs]
  [[:map [:ns-aliases map?]] sequential? '=> :any]
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

(defmethod eval-form "-" [ctx _ args]
  (ensure-nargs #{1 :vararg} args)
  (let [[ctx args] (eval-exprs ctx args)]
    {:expression [:binaryop {:op :subtract} [args]]
     :ctx ctx}))

(defmethod eval-form "+" [ctx _ args]
  (ensure-nargs #{1 :vararg} args)
  (let [[ctx args] (eval-exprs ctx args)]
    {:expression (case (count args)
                   0 [:number {:intpart "0"}]
                   1 (nth args 0)
                   [:binaryop {:op :add} [args]])
     :ctx ctx}))

(defmethod eval-form "*" [ctx _ args]
  (let [[ctx args] (eval-exprs ctx args)]
    {:expression (case (count args)
                   0 [:number {:intpart "1"}]
                   1 (nth args 0)
                   [:binaryop {:op :multiply} [args]])
     :ctx ctx}))

(defmethod eval-form "/" [ctx _ args]
  (ensure-nargs #{1 :vararg} args)
  (let [[ctx args] (eval-exprs ctx args)]
    {:expression (if (== 1 (count args))
                   [:binaryop {:op :divide} [[:number {:intpart "1"}] (nth args 0)]]
                   (reduce (fn [acc x]
                             [:binaryop {:op :divide} [acc x]])
                           args))
     :ctx ctx}))

(defmethod eval-form "import" [ctx _ args]
  (ensure-nargs 1 args)
  {:expression [:invoke "@import" [(nth args 0)]]
   :ctx ctx})

(defmethod eval-form "require" [ctx _ args]
  (ensure-nargs #{1 :vararg} args)
  (let [[ctx args] (eval-exprs ctx args)]
    {:ctx (reduce (fn [ctx decl]
                    (match decl
                      [:vector _ [[:string {:string dep}]
                                  [:keyword {:content "as"}]
                                  [:symbol {:symname alias-sym}]]]
                      (evalctx/add-require ctx dep {:alias alias-sym})))
                  ctx
                  args)
     :expression [:nil]}))

(defmethod eval-form "def" [ctx _ args]
  (ensure-nargs #{2} args)
  (let [[sym v] args
        sym (match sym
              [:symbol sym']
              sym')
        {:keys [expression ctx]} (compile-expr ctx v)]
    {:expression [:nil]
     :ctx (evalctx/add-global
           ctx
           [:assign :const sym expression])}))

(defmethod eval-form "defn" [ctx _ args]
  (ensure-nargs #{2 3 :vararg} args)
  (let [[sym params & body] args
        sym (match sym
              [:symbol sym']
              sym')
        [ctx body] (compile-exprs ctx body)]
    {:expression [:nil]
     :ctx (evalctx/add-fn
           ctx sym params body)}))

(defn restore-lexical-context [ctx]
  (assoc ctx :lexical (-> ctx :lexical :parent)))

(defmethod eval-form "let" [ctx _ body]
  (ensure-nargs #{2 :vararg} body)
  (match (first body)
    [:vector _ vec-items]
    (let [body (next body)
          pairs (partition 2 vec-items)
          {:keys [bindings ctx]}
          (reduce
           (fn [{:keys [ctx] :as acc} [sym v]]
             (match sym
               [:symbol {:symname sym}]
               (let [{:keys [expression ctx]} (compile-expr ctx v)]
                 (-> acc
                     (update :bindings conj
                             (clojure.lang.MapEntry. sym expression))
                     (assoc :ctx
                            (update-in ctx [:lexical :bindings]
                                       conj
                                       (clojure.lang.MapEntry. sym sym)))))))
           {:bindings {}
            :ctx ctx}
           pairs)
          [ctx body] (compile-exprs ctx body)]
      {:expression [:bind-local {:bindings bindings} body]
       :ctx (restore-lexical-context ctx)})))

(defmethod eval-form "if" [ctx _ body]
  (ensure-nargs 2 body)
  (let [[ctx [c t f]] (compile-exprs ctx body)]
    {:expression [:if c t f]
     :ctx ctx}))

(>defn eval-toplevel-form [ctx form]
  [[:map [:ns-aliases map?]] :any '=> :any]
  (match form
    [:list info [cmd & args]]
    (let [sym cmd]
      (eval-form ctx (:symname (second sym)) args))
    [:symbol {:symname symname}]
    {:ctx ctx
     :result nil}))

(defn eval-forms [ctx forms]
  (reduce (fn [ctx c]
            (:ctx (eval-toplevel-form ctx c)))
          ctx
          forms))

(comment
  ;; primitive lisp
  :bindings ;; symbolic bindings for ctx
  '(let [x 1
         y x]
     3)
  '(bind [_x [:code 1]]
         {:bindings {x [:local _x]}}
         (bind [_y [:code _x]]
               {:bindings {x [:local _x]
                           y [:local _y]}}
               [:code 3]))

  :bind-local
  (contains? (keys (methods eval-form)) "defn")

;; special forms
  'def
  "Creates a var"
  'if
  'do
  'let
  'quote
  'syntax-quote
  'var '??
  'fn
  "Creates a function (at comptime) returning a reference to it at runtime"
  'loop
  'recur
  'try

  (let [x (fn [a] a)]
    (x 4))

  ;; core operations / runtime forms

  '(with-local-binding [symbol <expr1>]
     <expr2>)
  ;; evals expr1 => result
  ;; result value is given the *alias* when evaluating expr2
  ;; zig variable is a unit of memory storage
  ;; variables are stack allocated (per stack frame).
  ;; If data is heap-allocated, would internally be a pointer
  ;; should the type annotation be on the symbol or the expr1?
  ;; data probably moves in memory space when returning from expr1 (moves stack frames)
  ;; variable should have a constant type - can be determined from initial value
  ;; makes less sense to annotate initial value if undefined - no data at all but container has type restriction

  '(resolve-binding sym)
  ;; If local: represents value at the local's memory address
  ;; If var: derefs the var (like a pointer) and returns the instantaneous value at a different memory address

  '(invoke <fn> <args>)

  '(lambda fnname [args+types..] expr)


  ;; Stages;
  ;; user :: code
  ;; reader :: pure data
  ;; (ES) eval - convert to special forms
  ;;   macroexpand untyped macros + typecheck :: analysis AST (special forms + macros)
  ;; staticanalysis - infer, typecheck :: fully annotated data
  ;;   macroexpand typed macros :: special forms
  ;; eval :: global ctx, refactor tree
  ;; emitter :: zig code

  ;; (ES), direct eval context
  ;; read first list form: if special symbol, execute special form. Else convert to `invoke`

  '(-> '(x) y)
  '(-> (quote (x)) y)
  '(macro -> [(invoke list (quote x)) (resolve-binding y)])
  '(invoke (resolve-binding y) [(invoke list (quote x))])


  ;; Zig Compiler forms
  '(do
     (defstatic x const :i32 5)
     (defn f [] nil (println [x]))
     (compile-deps! f) ;; recompiles all files with outstanding changes affecting f
     (execute f)
     (undef f)
     (compile-outstanding!))

  '(defstatic sym info type ZAST)
  "Adds a container-level variable"

  '(defn f args ret-type ZAST)
  "Adds a function"
  ;; parses code and may add additional anonymous functions

  '(undef sym)
  "Removes from eval ctx only if has no references"
  ;; marks the containing file to delete definition on next recompile

  '(execute fnsym)
  "Dynamically links and invokes function, returning result"

  ;; repl - evaluate runtime form
  '(do
     (defn __eval [] <input>)
     (compile-deps! __eval)
     (execute __eval))


#!
  )
