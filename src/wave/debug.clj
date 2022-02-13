(ns wave.debug
  (:require
   [crypticbutter.snoop :as snoop]
   [taoensso.timbre :as log]
   [taoensso.encore :as enc]
   [clojure.pprint :as pp]
   [malli.error :as me]))

(defn pp-log-middleware [data]
  (update data :vargs
          (fn [va]
            (loop [out (transient [])
                   ^clojure.lang.APersistentVector in va
                   pp-prev? false]
              (if (pos? (count in))
                (let [x (and in (.nth in 0))]
                  (if (or (map? x) (sequential? x) (set? x))
                    (let [^:string pretty (with-out-str (pp/pprint x))]
                      (if pp-prev?
                        (recur (conj! out pretty) (enc/vnext in) true)
                        (if (< 50 (.length pretty))
                          (recur (conj! out (str "\n" pretty))
                                 (enc/vnext in) true)
                          (recur (conj! out (subs pretty 0 (dec (.length pretty))))
                                 (enc/vnext in) false))))
                    (recur (conj! out x) (enc/vnext in) false)))
                (persistent! (conj! out \newline)))))))

(defn snoop-throw-validation-error
  "Default function used to throw errors when in/outstrumentation fails."
  [{:keys [explainer-error] :as data} boundary]
  (let [boundary-name (case boundary
                        :input "Instrument"
                        :output "Outstrument")
        data-str (fn [x]
                   (let [s (with-out-str (pp/pprint x))]
                     (if (< 350 (count s))
                       (str (subs s 0 350) " ... TRUNCATED")
                       x)))
        emsg-prefix (str boundary-name " error for:")
        fsym (symbol (str (:ns data))
                     (str (:name data)))
        _ (log/error emsg-prefix fsym)
        [got err] (enc/catching
                   (let [hm (me/humanize explainer-error)]
                     (case boundary
                       :input (let [idx (-> hm count dec)
                                    got (get-in explainer-error [:value idx])
                                    err (nth hm idx)]
                                (log/error "For param:" (nth (:params data) idx)
                                           "\nGot:" (data-str got)
                                           "\nError:" (data-str err))
                                [got err])
                       :output (let [got (:value explainer-error)]
                                 (log/error "Got:" (data-str got)
                                            "\nError:" (data-str hm))
                                 [got hm])))
                    _ (let [got (:value explainer-error)
                            err (:errors explainer-error)]
                        (log/error "Humanize failed"
                                   "\nGot:" (data-str got)
                                   "\nErrors:" (data-str err))
                        [got err]))]
    (tap> [emsg-prefix fsym got err])
    (throw (ex-info (str boundary-name " failed. See message printed above.") data))))

(defn install-logger! []
  (set! *print-length* 50)
  (set! *print-level* 7)
  (log/merge-config! {;:appenders {:println {:enabled? false}
                      ;            :cider-repl (cider-repl-appender)}
                      :middleware [#'pp-log-middleware]})
  (log/handle-uncaught-jvm-exceptions!)
  (swap! snoop/*config assoc
         :on-instrument-fail #(snoop-throw-validation-error % :input)
         :on-outstrument-fail #(snoop-throw-validation-error % :output)))

(comment
  (install-logger!)
  #!
  )
