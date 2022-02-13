(ns wave.type.cmn.ana
  (:require
   [tech.droit.fset :as fset]
   [com.rpl.specter :as sp]
   [clojure.core.match :refer [match]]
   [crypticbutter.snoop :refer [>defn]]
   [clojure.repl]
   [clojure.reflect :as reflect]))

(def NArgsSch [:or nat-int? [:= :varargs]])

(def ArityInfoSch [:map
                   [:params :any]
                   [:return :any]])

(defn max-reqarity [arityinfo]
  (reduce (fn [mx x]
            (if (= :varargs x) mx (max mx x)))
          0 (keys arityinfo)))

(defn arity->sig [arityinfo nargs]
  [[:map-of NArgsSch ArityInfoSch] nat-int? '=> [:maybe ArityInfoSch]]
  (get arityinfo nargs
       (let [maxreqarity (max-reqarity arityinfo)]
         (when (< maxreqarity nargs)
           (:varargs arityinfo)))))

(defn arity->sig-fallback
  "For when `nargs` is not a valid arity, a fallback signature is returned."
  [arityinfo nargs]
  [[:map-of NArgsSch ArityInfoSch] nat-int? '=> [:maybe ArityInfoSch]]
  (let [nmin (apply min (keys arityinfo))
        nmax (max-reqarity arityinfo)]
    (cond
      (< nargs nmin)
      (get arityinfo nmin)
      (< nmax nargs)
      (get arityinfo nmax))))
