(ns wave.repl
  (:require
   [wave.load :as load]
   [babashka.fs :as fs]
   [clojure.core.match :refer [match]]
   [clojure.zip :as zip]
   [wave.emit :as emit]
   [wave.evalctx :as evalctx]
   [wave.eval :as eval]
   [wave.eval2 :as eval2]
   [wave.reader :as reader]
   [clojure.java.io :as io]
   [wave.parser :as parser]
   [malli.core :as m]))

(comment
  (def ast
    (let [r (promise)
          t (Thread.
             (fn []
               (deliver r (with-open [rdr (io/reader "examples/hello/main.carp")]
                            (parser/read-next-form (parser/new-zipper) rdr)))))]
      (.start t)
      (let [res (deref r 300 :oops)]
        (if (= :oops res)
          (.stop t)
          res))))
  (def forms
    (let [r (promise)
          t (Thread.
             (fn []
               (deliver r (reader/read-file "examples/hello/main.carp"))))]
      (.start t)
      (let [res (deref r 300 :oops)]
        (if (= :oops res)
          (do (.stop t) :oops)
          res))))

  (-> (evalctx/new-evalctx)
      (eval2/eval-forms forms)
      emit-ctx)

  (defn emit-ctx [ctx]
    (emit/emit-unit ctx "main"
                    {:functions (vals (:functions ctx))
                     :globals (vals (:globals ctx))
                     :global-imports (mapv (fn [[var-name dep]]
                                             {:var-name var-name
                                              :dep dep})
                                           (:ns-aliases ctx))}))


  (load/compile-zig emit/target-dir-default "main")

  ;; '(defn bigfunction [something : String else-thing : i32 objectday]
  ;;    : void
  ;;    34 34 34 34)
  ;; '(defn : void bigfunction [: String something : i32 else-thing objectday]
  ;;    34 34 34 34)

  ;; '(defn : bigfunction void [: something String : else-thing i32 objectday]
  ;;    34 34 34 34)

  ;; '(defn (bigfunction void) [(something String) (else-thing i32) objectday]
  ;;    34 34 34 34)
  ;;
  )
