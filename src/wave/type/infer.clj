(ns wave.type.infer
  (:require
   [tech.droit.fset :as fset]
   [cascade.hike :as hike]
   [clojure.core.match :refer [match]]))

(defn free-variables [form]
  (match form
    [:value _ _]
    #{}
    [:variable _ _]
    #{form}
    [:invoke _ f args]
    (fset/union (free-variables f)
                (reduce fset/union (map free-variables args)))
    [:fn _ args body]
    (fset/difference
     (free-variables body)
     (reduce fset/union (map free-variables args)))
    [:bind _ _sym value]
    (free-variables value)))

(defn new-type-scheme [variables t]
  [:type-scheme variables t])

(defn type-variable-gensym [x]
  (str (gensym (str x "_t"))))

(defn instantiate [t]
  (match t
    [:type-scheme variables t']
    (let [subst (zipmap variables (map type-variable-gensym variables))]
      (hike/postwalk-replace subst t'))
    :else t))

#_(defn generalize
  [env t]
  (let [variables (vec (fset/difference (free-types t) (free-types env)))]
    (if (pos? (count variables))
      (new-type-scheme variables t)
      t)))

(comment
  ;; (free-variables)

  ;; type constants
  '(.
    i 1 65535
    u 1 65535
    isize
    usize
    c_short
    c_ushort
    c_int
    c_uint
    c_long
    c_ulong
    c_longlong
    c_ulonglong
    c_longdouble
    f16
    f32
    f64
    f128
    bool
    anyopaque
    void
    noreturn
    type
    anyerror
    comptime_int
    comptime_float)

  ;; zig primitive values
  ;; true, false, null, undefined

  '(assoc m k v)
  '[[:map-of k' v'] k' v']
  '[[:vector-of v'] idx-num? v']
  '[[:map k->v']
    [:as k' [:any]]
    [:as v' (or (k->v' k) (kw-val-type k))]]
  '[[:vector k->v']
    [:as k' idx-num?]
    [:as v' (k->v' k)]]

  {"s" 1}
  '[:map-of string? number?] [:map (fn [k'] )]

  :x/x
  [:keyword string?]
  {:x/x "s"}

  #!
  )
