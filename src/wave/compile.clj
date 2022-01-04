(ns wave.compile
  (:require
   [wave.load :as load]
   [clojure.java.shell :as shell]
   [wave.emit :as emit]
   [clojure.core.match :refer [match]]))

(defn cf-defstatic [ctx sym opts ty zast]
  (update ctx :pending conj
          [:defstatic sym opts ty zast]))

(defn compile-outstanding! [*ctx]
  (doseq [op (:pending @*ctx)]
    (match op
      [:defstatic sym opts ty zast]
      (swap! *ctx assoc-in [:globals sym] {:v zast
                                           :ty ty
                                           :prefix (:mut opts :const)
                                           :var-id sym})))
  (let [ctx @*ctx]
    (emit/emit-unit ctx "main"
                   {:functions (vals (:functions ctx))
                    :globals (vals (:globals ctx))
                    :global-imports (mapv (fn [[var-name dep]]
                                            {:var-name var-name
                                             :dep dep})
                                          (:ns-aliases ctx))})
    (doto (load/compile-zig (:target-dir ctx) "main")
      (-> :out println)
      (-> :err println))))

(defn execute! [ctx fnsym]
  #_(emit/emit-unit ctx "main"
                  {:functions (vals (:functions ctx))
                   :globals (vals (:globals ctx))
                   :global-imports (mapv (fn [[var-name dep]]
                                           {:var-name var-name
                                            :dep dep})
                                         (:ns-aliases ctx))}))

(defn run-cform! [*ctx cform]
  (match cform
    [:do _ forms]
    (doseq [f forms]
      (run-cform! *ctx f))
    [:cf _ :defstatic sym opts ty zast]
    (swap! *ctx cf-defstatic sym opts ty zast)
    [:cf _ :compile-outstanding!]
    (compile-outstanding! *ctx)))
