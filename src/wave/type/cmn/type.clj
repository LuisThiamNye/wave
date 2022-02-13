(ns wave.type.cmn.type
  (:require
   [wave.util :as util]
   [crypticbutter.snoop :refer [>defn]]
   [wave.type.cmn.ctx :as ctx]
   [tech.droit.fset :as fset]))

(def Schema
  [:vector vector?])

(defn add-free [ctx id]
  (-> ctx
      (update :frees assoc id {:type []})))

(defn add-free-binding [ctx sym]
  (let [id (str (gensym (str "free." sym ".")))]
    [(add-free (update ctx :lexical-bindings assoc sym {:lexical-id [:frees id]})
               id)
     id]))

(>defn instantiate-frees [ctx overrides mode csts]
  [map? map? keyword? vector? '=> vector?]
  (loop [i' (count csts)
         ctx ctx
         result csts
         newfrees overrides]
    (let [i (dec i')]
      (if (neg? i)
        [ctx result newfrees]
        (let [cst (nth csts i)]
          (if (= :free-read (nth cst 0))
            (let [fixedid (nth cst 1)
                  existingid (get newfrees fixedid)
                  id (or existingid
                         (-> (re-matches #"([^.]+\.[^.]+\.).+" fixedid)
                          second (str "inst.")
                          gensym str))
                  result (conj result
                               [(case mode
                                  :read :free-read
                                  :join :free-join)
                                id])]
              (if existingid
                (recur i ctx result newfrees)
                (recur i (add-free ctx id)
                       result (assoc newfrees fixedid id))))
            (recur i ctx result newfrees)))))))

(defn cancel-out-csts [cs1 cs2]
  (let [cs1 (set cs1) cs2 (set cs2)]
    [(fset/difference cs1 cs2) (fset/difference cs2 cs1)]))

(defn type-errors [ctx limit-type target-type]
  ((util/get-meta-fn ctx `type-errors) ctx limit-type target-type))

(>defn join-types
  "Adds constraints to free variables to narrow each type
  to their common 'width'."
  [ctx limit-type target-type]
  [map? Schema Schema
   '=> [:tuple ctx/Schema [:map [:errors vector?]]]]
  (let [errors (type-errors ctx limit-type target-type)]
    (if (pos? (count errors))
     [ctx
      {:errors [{:e-type :type-mismatch
                 :got target-type
                 :expected limit-type
                 :errors (vec errors)}]}]
     (let [[cs1 cs2] (cancel-out-csts limit-type target-type)
           join-side
           (fn join-side [ctx lhs rhs]
             (if-let [lid (some (fn [cst]
                                  (when (= :free-join (nth cst 0))
                                    (nth cst 1)))
                                lhs)]
               (let [lfcs (set (get-in ctx [:frees lid :type]))
                     rcs (into #{}
                               (map (fn [cst]
                                      (cond-> cst (= :free-join (nth cst 0))
                                              (assoc 0 :free-read))))
                               rhs)
                     res (vec (fset/union lfcs rcs))]
                 (prn lhs)
                 (assoc-in ctx [:frees lid :type] res))
               ctx))]
       [(-> ctx
            (join-side cs1 cs2)
            (join-side cs2 cs1))
        {:errors []}]))))

(defn join-concrete-types [t1 t2]
  ;; TODO
  (vec (fset/union t1 t2)))

(>defn concrete-type [ctx t]
  (reduce (fn [res [variant :as cst]]
            (if (#{:free-read} variant)
              (let [ftype (get-in ctx [:frees (nth cst 1) :type])]
                (join-concrete-types res ftype))
              (conj res cst)))
          []
          t))
