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

(def ^:private non-code-groups
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
    token-data))

(declare read-structured-token)

(defn- wrap-coll [data]
  (let [first-meta (-> data first meta)
        last-meta (-> data last meta)]
    (vary-meta (into [:collection] data)
      assoc
      :start-line (:start-line first-meta)
      :start-column (:start-column first-meta)
      :end-line (:end-line last-meta)
      :end-column (:end-column last-meta)
      :indent (:indent first-meta))))

(defn- add-delim [data end-delim {:keys [*line *column]}]
  (let [line @*line
        column @*column
        token-data (vary-meta [:delimiter end-delim] assoc
                     :start-line line
                     :start-column column
                     :end-line line
                     :end-column (+ column (count end-delim)))
        [last-data first-data]
        (->> data
             reverse
             (split-with #(-> % first non-code-groups))
             (mapv reverse)
             (mapv vec))]
    (into (conj first-data token-data)
          last-data)))

(defn- read-coll [flat-tokens [_ delim :as token-data] {:keys [*index parinfer] :as opts}]
  (let [end-delim (delims delim)
        indent (-> token-data meta :indent)]
    (loop [data [token-data]]
      (if-let [[_ token :as token-data] (read-structured-token flat-tokens opts)]
        (cond
          (and (= :indent parinfer)
               (< (-> token-data meta :indent) indent))
          (do
            (vswap! *index dec) ;; read token again later
            (wrap-coll (add-delim data end-delim opts)))
          (= token end-delim)
          (wrap-coll (conj data token-data))
          (close-delims token)
          (if (= :indent parinfer)
            (wrap-coll (add-delim data end-delim opts))
            (vary-meta (wrap-coll (conj data token-data))
              assoc :error-message "Unmatched delimiter"))
          :else
          (recur (conj data token-data)))
        (if (= :indent parinfer)
          (wrap-coll (add-delim data end-delim opts))
          (vary-meta (wrap-coll data)
            assoc :error-message "EOF while reading"))))))

(defn- read-structured-token [flat-tokens {:keys [*line *column *indent *index] :as opts}]
  (when-let [[group token :as token-data] (get flat-tokens (vswap! *index inc))]
    (let [start-line @*line
          start-column @*column
          end-line (if (= group :newline-and-indent)
                     (vswap! *line inc)
                     @*line)
          end-column (if (= group :newline-and-indent)
                       (vreset! *column (dec (count token)))
                       (vswap! *column + (count token)))
          indent (cond
                   (open-delims token)
                   (vreset! *indent end-column)
                   (= group :newline-and-indent)
                   (vreset! *indent (dec (count token)))
                   :else
                   @*indent)
          token-data (vary-meta token-data assoc
                       :start-line start-line
                       :start-column start-column
                       :end-line end-line
                       :end-column end-column
                       :indent indent)]
      (if (open-delims token)
        (read-coll flat-tokens token-data opts)
        token-data))))

(defn- read-useful-token [flat-tokens {:keys [parinfer] :as opts}]
  (when-let [[_ token :as token-data] (read-structured-token flat-tokens opts)]
    (cond
      (close-delims token)
      (if (= :indent parinfer)
        (read-useful-token flat-tokens opts)
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
                :*line (volatile! 0)
                :*column (volatile! 0)
                :*indent (volatile! 0)
                :*index (volatile! -1))]
     (loop [structured-tokens []]
       (if-let [token-data (read-useful-token tokens opts)]
         (recur (conj structured-tokens token-data))
         structured-tokens)))))

(defn- node->str [node]
  (if (vector? node)
    (let [[type & children] node]
      (str/join (map node->str children)))
    node))

(defn flatten [parsed-code]
  (->> parsed-code
       (mapv node->str)
       str/join))

