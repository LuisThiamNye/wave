(ns wave.reader
  (:refer-clojure :exclude [read])
  (:require
   [wave.parser :as parser]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [babashka.fs :as fs]
   [clojure.string :as str]))

(defprotocol LispFileReaderP
  (getLine [_])
  (getCol [_])
  (read [_]))

(deftype LispFileReader
  [^java.io.PushbackReader reader
   ^String filepath
   ^:unsynchronized-mutable ^long line
   ^:unsynchronized-mutable ^long col]
  LispFileReaderP
  (getLine [_] line)
  (getCol [_] col)
  (read [_]
    (let [c (.read reader)]
      (case c
        10 (do
             (set! line (inc line))
             (set! col 0)
             c)
        13 c
        (do
          (set! col (inc col))
          c))))
  java.io.Closeable
  (close [_]
    (.close reader)))

(defn new-file-reader [src]
  (LispFileReader.
   (java.io.PushbackReader. (io/reader src))
   src
   1 0))

(defn current-pos [^LispFileReader rdr]
  {:line (.getLine rdr)
   :col (.getCol rdr)})

(defn reader-filepath [^LispFileReader rdr]
  (.-filepath rdr))

#_(defn reader-done? [^java.io.PushbackReader reader]
  (let [c (read-non-whitespace reader)]
    (if (nil? c)
      true
      (do (.unread reader c)
          false))))

(defn try-read-next [^LispFileReader rdr]
  (let [x (.read rdr)]
    (when-not (== -1 x)
      x)))

(defn read-next [rdr]
  (or (try-read-next rdr)
      (throw (Exception. "Unexpected end of file"))))

(defn unread [^LispFileReader rdr c]
  (.unread (.-reader rdr) c))

(def whitespace-charcodes
  (into #{} (map int)
        [\space \newline \return \tab]))

(defn read-non-whitespace [reader]
  (when-let [c (try-read-next reader)]
    (if (contains? whitespace-charcodes c)
      (recur reader)
      c)))

#_
(defn read-file [source]
  (with-open [r' (io/reader source)]
    (let [reader (java.io.PushbackReader. r')]
      (loop [r []]
        (if (reader-done? reader)
          r
          (let [e (edn/read reader)]
            (recur (conj r e))))))))

(defn read-symbol [rdr]
  (with-open [w (java.io.StringWriter.)]
    (loop []
      (let [c (try-read-next rdr)]
        (if (or (nil? c) (parser/symbol-break-char? c))
          (do (unread rdr c)
              (str w))
          (do (.write w c)
              (recur)))))))

(defn read-number-literal [rdr]
  #_(let [[o c] (parser/read-number-literal rdr (read-next rdr))]
    (unread rdr c)
    o)
  (let [[i f] (str/split (read-symbol rdr) #"\.")]
    {:intpart i
     :fracpart f}))

(defn string-reader-default [{:keys [read-next-code write-char]} _c]
  (if-let [real-char
           (case (char (read-next-code))
             \n \newline
             \t \tab
             \b \backspace
             \r \return
             \f \formfeed
             \\ \\ ;;
             )]
    (do (write-char real-char) true)
    false))

(defn read-string-literal [rdr]
  (with-open [w (java.io.StringWriter.)]
    (let [read-next' (fn []
                      (or (try-read-next rdr)
                          (throw (Exception. "Unexpected end of string"))))
          api {:read-next-code read-next'
               :write-char #(.write w %)}]
      (loop []
        (let [c (read-next rdr)]
          (case (char c)
            \" (str w)
            (do #_(if-let [cust-readers ({92 [string-reader-default]} c)]
                  (loop [i (dec (count cust-readers))]
                    (when (and (pos? i) (not ((nth cust-readers i) api c)))
                      (recur (dec i))))
                  (.write w c))
                (.write w c)
                (recur))))))))

(declare read-form)

(defn read-delimited-list [delim ctx rdr]
  (loop [out []]
    (if-let [out (conj out (read-form ctx rdr))]
      (let [c (read-next rdr)]
        (if (== (int delim) c)
          out
          (recur out)))
      (throw (Exception. "Unexpected end of file")))))

(defn read-form [ctx rdr]
  (let [pos (volatile! (current-pos rdr))
        set-pos! (fn [] (vreset! pos (current-pos rdr)))]
    (some->
     (loop []
      (when-let [c (read-non-whitespace rdr)]
        (set-pos!)
        (case (char c)
          \(
          [:list {} (read-delimited-list \) ctx rdr)]
          \[
          [:vector {} (read-delimited-list \] ctx rdr)]
          \{
          [:map {} (read-delimited-list \} ctx rdr)]
          \:
          (let [value (read-symbol rdr)]
            [:keyword {} value])
          \;
          (and (loop []
                 (let [c (read-next rdr)]
                   (cond
                     (== 10 c) true
                     (== -1 c) nil
                     :else (recur))))
               (recur))
          \"
          [:string {} (read-string-literal rdr)]
          ;; ELSE
          (do (unread rdr c)
              (if (parser/ascii-number? c)
                (let [num (read-number-literal rdr)]
                  [:number num])
                (let [sym (read-symbol rdr)]
                  [:symbol {} sym]))))))
    (as-> node
      (let [{:keys [line col]} @pos]
        (update node 1 assoc
                :line line
                :col col))))))

(defn read-file [src]
  (with-open [rdr (new-file-reader src)]
    (loop [forms []]
      (if-let [f (read-form {} rdr)]
        (recur (conj forms f))
        forms))))

(comment
  (read-file "examples/hello/main.carp")
;; => [(require ' ["std" :as std])
;;     (defn main [] (std/debug.print "Hello {s}\n" ["world"]))]
  #!
  )
(comment
  ;; reader forms
  [:symbol {} "sym"]
  [:list {} []]
  [:vector {} []]
  [:map {} {}]
  ;; -- literals
  [:string {} "str"]
  [:number {} "0x4js8"]
  [:keyword {} "k"]

  ;; reader macros
  ;; ' (quote <_>)
  ;; ; comment
  ;; @ (deref <_>)
  ;; # dispatch
  ;; ` (syntax-quote <_>)
  ;; ~ unquote
  ;; |
  ;; !
  ;; :
  ;; %
  ;; $
  ;; ^
  ;; &
  ;; \
  ;; ?
  ;; ,
  ;; .

  ;; dispatches
  ;; _ ignore
  ;; / generic type

  #!
  )
