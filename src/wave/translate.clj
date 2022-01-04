(ns wave.translate
  (:require
   [wave.eval2 :as eval2]
   [wave.evalctx :as evalctx]
   [clojure.core.match :refer [match]]))

(defn translate-rtcode
  "Converts runtime block SAST to Zig AST. returns [ctx zast]"
  [ctx sast]
  (match sast
    [:lambda _opts sym params expr]
    [(evalctx/add-fn ctx
                     (if-let [p (-> ctx :lexical :parent-fn-sym)]
                       (str p "__fn_" sym)
                       sym)
                     params
                     (translate-rtcode
                      (eval2/update-lexical-context
                       ctx (fn [l]
                             (assoc l :parent-fn-sym sym)))
                      expr))
     [:symbol {} sym]] ;; returns reference to the function
    ))
