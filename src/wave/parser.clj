(ns wave.parser
  (:require
   [crypticbutter.snoop :refer [>defn]]
   [clojure.zip :as zip]
   [clojure.string :as str]
   [clojure.java.io :as io]
   [malli.core :as m]))

(def FnParamsVecSchema
  [:and vector?
   [:*
    [:altn
     [:param :any]]]])

(def ArityDeclSchema
  [:alt
   [:catn
    [:params-decl FnParamsVecSchema]
    [:prepost-map [:? map?]]
    [:body [:+ :any]]]
   [:catn
    [:params-decl FnParamsVecSchema]
    [:prepost-map [:? map?]]
    [:body [:* :any]]]])

(def DefnSchema
  [:catn
   [:sym symbol?]
   [:docstring [:? string?]]
   [:attr-map [:? map?]]
   [:code
    [:altn
     [:multi-arity
      [:catn
       [:defs [:+ [:and list? ArityDeclSchema]]]
       [:attr-map [:? map?]]]]
     [:single-arity ArityDeclSchema]]]])

(defn new-zipper []
  (zip/zipper (fn branch? [x] (nth x 2 false))
              (fn children [branch-node]
                (nth branch-node 2))
              (fn make-node [node children]
                (assoc node 2 (vec children)))
              [:toplevel {} []]))

(def whitespace-chars #{\space \newline \return \tab})

(defn try-read-next [^java.io.Reader reader]
  (let [x (.read reader)]
    (when-not (== -1 x)
      x)))

(defn read-next [^java.io.Reader reader]
  (or (try-read-next reader)
      (throw (Exception. "unexpected end of file"))))

(defn read-non-whitespace
  ([reader c]
   (if (and (some? c) (contains? whitespace-chars (char c)))
     (when-let [c2 (try-read-next reader)]
       (recur reader c2))
     c))
  ([reader]
   (read-non-whitespace reader (try-read-next reader))))

(defn read-ignore-form [reader]
  (let [close-bracket (fn close-bracket [brackets typ]
                        (if (= typ (peek brackets))
                          (pop brackets)
                          (throw (Exception. "unbalanced brackets"))))]
    (loop [c (read-non-whitespace reader)
           brackets []]
      (case c
        \#
        nil ;; TODO
        (let [next-brackets (case c
                              \) (close-bracket brackets :round)
                              \( (conj brackets :round)
                              \] (close-bracket brackets :square)
                              \[ (conj brackets :square)
                              \} (close-bracket brackets :curly)
                              \{ (conj brackets :curly))
              c2 (char (read-next reader))]
          (when-not (contains? whitespace-chars c2)
            (recur c2 next-brackets)))))))

(defn read-string-literal [reader]
  (with-open [w (java.io.StringWriter.)]
    (loop [c (read-next reader)]
      (if c
        (case (char c)
          ;; \\ (do (.append w (case (char (read-next reader))
          ;;                     \n \newline
          ;;                     \t \tab
          ;;                     \b \backspace
          ;;                     \r \return
          ;;                     \f \formfeed
          ;;                     \\ \\))
          ;;        (recur (read-next reader)))
          \" (recur nil)
          (do (.write w c)
              (recur (read-next reader))))
        (str w)))))

(def bracket-chars #{\( \) \[ \] \{ \}})

(defn symbol-break-char? [c]
  (let [c (char c)]
    (or (contains? whitespace-chars c)
        (contains? bracket-chars c))))

(defn read-symbol [reader c]
  (with-open [w (java.io.StringWriter.)]
    (loop [c c]
      (if (or (nil? c) (symbol-break-char? c))
        [(str w) c]
        (do (.write w c)
            (recur (read-next reader)))))))

(defn ascii-number? [n] (<= 48 n 57))
(def ^:const ASCII-DOT 46)

(defn write-from-reader-while [w pred reader init]
  (loop [c init]
    (if (and (some? c) (pred c))
      (do (.write w c)
          (recur (read-next reader)))
      c)))

(defn read-chars-where [pred ignore? reader c]
  (with-open [w (java.io.StringWriter.)]
    (loop [c c]
      (if (nil? c)
        [(str w) nil]
        (let [c (write-from-reader-while w pred reader c)]
          (if (ignore? c)
            (recur (read-next reader))
            [(str w) c]))))))

(defn ascii-hex? [x]
  (or (ascii-number? x)
      (<= 97 x 102)
      (<= 65 x 70)))

(defn read-number-suffix- [reader c]
  (let [suf (case c
              (\e \E)
              (let [c (read-next reader)
                    is-neg? (= \- (char c))
                    c (if is-neg? (char (read-next reader)) c)
                    power (read-chars-where
                           ascii-number?
                           (fn [_] false)
                           reader c)]
                [[:exp {:power power
                        :negative? is-neg?}]
                 (char (read-next reader))])
              \N :arb-int
              \M :arb-float
              nil)]
    (if (vector? suf)
      suf
      [suf (if (nil? suf)
             c
             (char (read-next reader)))])))

(defn read-number-suffix [reader c]
  (if (= \* c)
    (read-number-suffix- reader (char (read-next reader)))
    (read-number-suffix- reader c)))

(defn read-number-literal-of-radix [reader c valid-char? radix]
  (let [[intpart c] (read-chars-where
                     valid-char?
                     #(= \_ (char %))
                     reader c)
        [fracpart c] (if (= \. c)
                       (read-chars-where
                        valid-char?
                        #(= \_ (char %))
                        reader (char (read-next reader)))
                       [nil c])
        [suffix c] (read-number-suffix reader c)]
    [{:intpart intpart
      :fracpart fracpart
      :radix radix
      :suffix suffix} c]))

(defn read-number-literal- [reader c]
  (let [[x c] (read-chars-where ascii-number? #(= \_ (char %)) reader c)
        [radix valid-char? intpart c]
        (if (= \r c)
          (let [radix (parse-long x)]
            (if (<= 2 radix 36)
              (let [max-uc (+ 54 radix)
                    max-lc (+ 86 radix)
                    valid-char? #(or (ascii-number? %)
                                     (<= 97 % max-lc)
                                     (<= 65 % max-uc))]
                [radix
                 valid-char?
                 (read-chars-where
                  valid-char?
                  #(= \_ (char %)) reader c)
                 (char (read-next reader))])
              (throw (Exception. "Radix out of range"))))
          [nil ascii-number? x c])
        [fracpart c] (if (= \. c)
                       (read-chars-where
                        valid-char?
                        #(= \_ (char %))
                        reader (char (read-next reader)))
                       [nil c])
        [suffix c] (read-number-suffix reader c)]
    [{:intpart intpart
      :fracpart fracpart
      :radix radix
      :suffix suffix} c]))

(>defn read-number-literal [reader c]
 [:any some? '=> [:tuple [:map [:intpart string?]] :any]]
  (if (= \0 c)
    (let [c (char (read-next reader))]
      (case c
        \x (read-number-literal-of-radix reader c ascii-hex? 16)
        \b (read-number-literal-of-radix reader c #(<= 48 % 49) 2)
        \o (read-number-literal-of-radix reader c #(<= 48 % 55) 8)
        (read-number-literal- reader c)))
    (read-number-literal- reader c)))

(defn maybe-end-quote [ctx]
  (let [node (zip/node ctx)]
    (cond-> ctx
     (and (= :quote (nth node 0))
          (:sugar? (nth node 1))
          (== 1 (count (nth node 2))))
     zip/up)))

(defn append-child [ctx child]
  (let [node (zip/node ctx)]
    (-> ctx
        maybe-end-quote
        (zip/append-child child))))

(defn ctx-close-bracket [type' ctx]
  (let [node (zip/node ctx)]
    (if (= ({:curly :map
             :square :vector
             :round :list} type')
           (nth node 0))
      (maybe-end-quote (zip/up ctx))
      (do (prn node)
          (throw (Exception. (str "Wrong closing bracket: " type' " for " (nth node 0))))))))

(defn read-next-form
  ([ctx reader]
   (let [c (read-non-whitespace reader)]
     (read-next-form ctx reader c)))
  ([ctx reader c]
   (if (nil? c)
     (do (when-not (= :toplevel (nth (zip/node ctx) 0))
           (prn (zip/node ctx))
           (throw (Exception. "Unbalanced")))
       ctx)
     (case (char c)
       \(
       (recur (-> ctx
                  (append-child [:list {} []])
                  (zip/down) zip/rightmost)
              reader
              (read-non-whitespace reader))
       \[
       (recur (-> ctx
                  (append-child [:vector {} []])
                  (zip/down) zip/rightmost)
              reader
              (read-non-whitespace reader))
       \{
       (recur (-> ctx
                  (append-child [:map {} []])
                  (zip/down)
                  zip/rightmost)
              reader
              (read-non-whitespace reader))
       \)
       (recur (ctx-close-bracket :round ctx)
              reader
              (read-non-whitespace reader))
       \]
       (recur (ctx-close-bracket :square ctx)
              reader
              (read-non-whitespace reader))
       \}
       (recur (ctx-close-bracket :curly ctx)
              reader
              (read-non-whitespace reader))
       \:
       (let [[keytext c] (read-symbol reader (read-next reader))]
         (recur (-> ctx
                    (append-child [:keyword {:content keytext}]))
                reader (read-non-whitespace reader c)))
       \;
       (do (while (not (== 10 (read-next reader))))
           (recur ctx reader (read-non-whitespace reader)))
       \"
       (recur (-> ctx
                  (append-child [:string {:string (read-string-literal reader)}]))
              reader (read-non-whitespace reader))
       \#
       (let [c (char (read-next reader))]
         (cond
           (= \_ c)
           (do (read-ignore-form reader)
               (recur ctx reader (read-non-whitespace reader)))))
       \'
       (recur (-> ctx
                  (append-child [:quote {:sugar? true} []])
                  zip/down zip/rightmost)
              reader (read-non-whitespace reader))
       (if (ascii-number? (int c))
         (let [[num c] (read-number-literal reader c)]
           (recur (-> ctx
                      (append-child [:number num]))
                  reader (read-non-whitespace reader c)))
         (let [[sym c] (read-symbol reader c)]
           (recur (-> ctx
                      (append-child [:symbol {:symname sym}]))
                  reader (read-non-whitespace reader c))))))))

(comment

;;
  )
