(ns wave.clj.core
  (:require
   [wave.clj.compile :as clj.compile]
   [wave.evalctx :as evalctx]))

(defn new-evalctx [& {:as opts}]
  (assoc (evalctx/new-evalctx opts)
         :clj-macros {}))

(def ^:dynamic *ctx* (new-evalctx))

(defn defnz [sym params & body]
  (swap! *ctx*
         (fn [ctx]
           (let [aast (clj.compile/desugar-form ctx (cons 'do body))
                 ;;faast TODO static analysis
                 ;;sast TODO translate
                 zast (translate/translate-rtcode aast)]
             (evalctx/add-fn ctx (str sym) params zast)))))

(comment

  #!
  )
