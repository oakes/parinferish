(ns paren-salsa.core
  (:require [clojure.string :as str])
  (:refer-clojure :exclude [find flatten])
  #?(:clj (:import [java.util.regex Pattern Matcher])))

(def ^:private groups
  [[:newline-and-indent "(\n[ ]*)"]
   [:whitespace         "([\\s,]+)"]
   [:special-char       "(~@|['`~^@])"]
   [:delimiter          "([\\[\\]{}()]|#\\{)"]
   [:string             "(\"(?:\\\\.|[^\\\\\"])*\"?)"]
   [:character          "(\\\\\\S)"]
   [:backslash          "(\\\\)"]
   [:comment            "(;.*)"]
   [:number             "(\\d+\\.?[a-zA-Z\\d]*)"]
   [:symbol             "([^\\s\\[\\]{}('\"`,;)\\\\]+)"]])

(def ^:private whitespace?
  #{:newline-and-indent :whitespace :comment})

(def ^:private group-range (range 0 (count groups)))

(def ^:private regex-str (->> groups (map second) (str/join "|")))
(def ^:private regex (re-pattern regex-str))

(def ^:private open-delims #{"#{" "(" "[" "{"})
(def ^:private close-delims #{"}" ")" "]"})
(def ^:private delims {"#{" "}"
                       "(" ")"
                       "[" "]"
                       "{" "}"})

(defprotocol IRegex
  (find [this])
  (group [this index])
  (index [this]))

(defn ->regex [match-str]
  (let [regex #?(:clj  regex
                 :cljs (js/RegExp. regex-str "g"))
        *matcher #?(:clj (.matcher ^Pattern regex match-str)
                    :cljs (volatile! (make-array 0)))]
    (reify IRegex
      (find [this]
        #?(:clj  (re-find *matcher)
           :cljs (when @*matcher (vreset! *matcher (.exec regex match-str)))))
      (group [this index]
        #?(:clj  (.group ^Matcher *matcher ^int index)
           :cljs (aget @*matcher index)))
      (index [this]
        #?(:clj  (.start ^Matcher *matcher)
           :cljs (.-index @*matcher))))))

(defn- read-token [matcher *error?]
  (let [token (group matcher 0)
        group (get-in groups
                [(some #(when (group matcher (inc %)) %) group-range) 0]
                :whitespace)
        token-data [(if (and (= group :symbol)
                             (str/starts-with? token ":"))
                      :keyword
                      group)
                    token]]
    (when (and (= group :string)
               (not (str/ends-with? token "\"")))
      (vreset! *error? true))
    (cond-> token-data
            (whitespace? group)
            (vary-meta assoc :whitespace? true))))

(declare read-structured-token)

(defn- wrap-coll [data]
  (let [first-meta (-> data first meta)]
    (vary-meta (into [:collection] data)
      assoc :indent (:indent first-meta))))

(defn- insert-token [data token-data]
  (conj data (vary-meta token-data assoc :action :insert)))

(defn- remove-token [data token-data]
  (conj data (vary-meta token-data assoc :action :remove)))

(defn- insert-delim [data end-delim]
  (let [first-meta (-> data first meta)
        token-data (vary-meta [:delimiter end-delim] assoc
                     :indent (:indent first-meta)
                     :action :insert)
        [last-data first-data]
        (->> data
             reverse
             (split-with #(-> % first whitespace?))
             (mapv reverse)
             (mapv vec))]
    (into (conj first-data token-data)
          last-data)))

(defn- read-next-tokens-with-indent [flat-tokens {:keys [*index] :as opts} indent]
  (loop [tokens []]
    (if-let [[group _ :as token-data] (read-structured-token flat-tokens opts)]
      (if (>= (-> token-data meta :indent) indent)
        (recur (conj tokens (cond-> token-data
                                    (= group :delimiter)
                                    (vary-meta assoc :action :remove))))
        (do
          (vswap! *index dec)
          tokens))
      tokens)))

(defn- read-next-useful-tokens-with-indent [flat-tokens {:keys [*index] :as opts} indent]
  (let [tokens (read-next-tokens-with-indent flat-tokens opts indent)]
    (if (and (seq tokens)
             (->> tokens (map first) (every? whitespace?)))
      (do
        (vswap! *index - (count tokens))
        [])
      tokens)))

(defn- read-coll-indent-mode [flat-tokens [_ delim :as token-data] {:keys [*index] :as opts}]
  (let [end-delim (delims delim)
        indent (-> token-data meta :indent)]
    (loop [data [token-data]]
      (if-let [[_ token :as token-data] (read-structured-token flat-tokens opts)]
        (cond
          (< (-> token-data meta :indent) indent)
          (do
            (vswap! *index dec) ;; read token again later
            (-> data
                (remove-token token-data)
                (insert-delim end-delim)
                wrap-coll))
          (= token end-delim)
          (let [tokens-to-move (read-next-useful-tokens-with-indent flat-tokens opts indent)]
            (if (seq tokens-to-move)
              (-> data
                  (remove-token token-data)
                  (into tokens-to-move)
                  (insert-token token-data)
                  wrap-coll)
              (-> data
                  (conj token-data)
                  wrap-coll)))
          (close-delims token)
          (-> data
              (remove-token token-data)
              (insert-delim end-delim)
              wrap-coll)
          :else
          (recur (conj data token-data)))
        (-> data
            (insert-delim end-delim)
            wrap-coll)))))

(defn- read-coll [flat-tokens [_ delim :as token-data] {:keys [*index] :as opts}]
  (let [end-delim (delims delim)]
    (loop [data [token-data]]
      (if-let [[_ token :as token-data] (read-structured-token flat-tokens opts)]
        (cond
          (= token end-delim)
          (wrap-coll (conj data token-data))
          (close-delims token)
          (vary-meta (wrap-coll (conj data token-data))
            assoc :error-message "Unmatched delimiter")
          :else
          (recur (conj data token-data)))
        (vary-meta (wrap-coll data)
          assoc :error-message "EOF while reading")))))

(defn- read-structured-token [flat-tokens {:keys [*column *indent *index parinfer] :as opts}]
  (when-let [[group token :as token-data] (get flat-tokens (vswap! *index inc))]
    (let [end-column (if (= group :newline-and-indent)
                       (vreset! *column (dec (count token)))
                       (vswap! *column + (count token)))
          indent (cond
                   (open-delims token)
                   (vreset! *indent end-column)
                   (= group :newline-and-indent)
                   (vreset! *indent (dec (count token)))
                   :else
                   @*indent)
          token-data (vary-meta token-data assoc :indent indent)]
      (if (open-delims token)
        (if (= :indent parinfer)
          (read-coll-indent-mode flat-tokens token-data opts)
          (read-coll flat-tokens token-data opts))
        token-data))))

(defn- read-useful-token [flat-tokens {:keys [parinfer] :as opts}]
  (when-let [[_ token :as token-data] (read-structured-token flat-tokens opts)]
    (cond
      (close-delims token)
      (if (= :indent parinfer)
        (vary-meta token-data assoc :action :remove)
        (vary-meta token-data assoc :error-message "Unmatched delimiter"))
      :else
      token-data)))

(defn parse
  ([s]
   (parse s {}))
  ([s opts]
   (let [matcher (->regex s)
         *error? (volatile! false)
         tokens (loop [tokens (transient [])]
                  (if (find matcher)
                    (recur (conj! tokens (read-token matcher *error?)))
                    (persistent! tokens)))
         opts (cond-> opts @*error? (dissoc :parinfer))
         opts (assoc opts
                :*column (volatile! 0)
                :*indent (volatile! 0)
                :*index (volatile! -1))]
     (loop [structured-tokens []]
       (if-let [token-data (read-useful-token tokens opts)]
         (recur (conj structured-tokens token-data))
         structured-tokens)))))

(defn- node-iter [node-fn nodes node]
  (if (= (first node) :collection)
    (->> (reduce
           (fn [v child]
             (node-iter node-fn v child))
           []
           (rest node))
         (into (with-meta [:collection] (meta node)))
         node-fn
         (conj nodes))
    (if-not (-> node meta :action (= :remove))
      (conj nodes (node-fn node))
      nodes)))

(defn flatten
  ([parsed-code]
   (->> parsed-code
        (flatten #(-> % rest str/join))
        str/join))
  ([node-fn parsed-code]
   (reduce
     (fn [v code]
       (into v (node-iter node-fn [] code)))
     []
     parsed-code)))

(defn- diff-node [*line *column *diff node-meta node]
  (if (vector? node)
    (let [[type & children] node]
      (if (= type :newline-and-indent)
        (do
          (vswap! *line inc)
          (vreset! *column (-> children first count dec)))
        (run! (partial diff-node *line *column *diff (meta node)) children)))
    (let [line @*line
          column @*column]
      (vswap! *column + (count node))
      (when-let [action (:action node-meta)]
        (vswap! *diff conj {:line line :column column :content node :action action})
        (when (= action :remove)
          (vswap! *column - (count node)))))))

(defn diff [parsed-code]
  (let [*line (volatile! 0)
        *column (volatile! 0)
        *diff (volatile! [])]
    (run! (partial diff-node *line *column *diff nil) parsed-code)
    @*diff))

