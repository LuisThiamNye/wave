(ns wave.util)

(defn get-meta-fn [obj fnsym]
  (get (meta obj) fnsym))
