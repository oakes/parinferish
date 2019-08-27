(ns parinferish.core
  (:require [clojure.string :as str])
  (:refer-clojure :exclude [find flatten])
  #?(:clj (:import [java.util.regex Pattern Matcher])))

(def ^:private groups
  [[:newline-and-indent "(\n[ ]*)"]
   [:whitespace         "([ \\t\\r,]+)"]
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

(defn- read-token [matcher *error? *line *column *indent]
  (let [token (group matcher 0)
        group (get-in groups
                [(some #(when (group matcher (inc %)) %) group-range) 0]
                :whitespace)
        token-data [(if (and (= group :symbol)
                             (str/starts-with? token ":"))
                      :keyword
                      group)
                    token]
        line (if (= group :newline-and-indent)
               (vswap! *line inc)
               @*line)
        start-column (if (= group :newline-and-indent)
                       -1
                       @*column)
        end-column (if (= group :newline-and-indent)
                     (vreset! *column (dec (count token)))
                     (vswap! *column + (count token)))
        indent (cond
                 (and (= :delimiter group)
                      (open-delims token))
                 (vreset! *indent end-column)
                 (= group :newline-and-indent)
                 (vreset! *indent (dec (count token)))
                 :else
                 @*indent)
        token-data (vary-meta token-data assoc
                     :line line
                     :column start-column
                     :indent indent)]
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

(defn read-next-tokens-with-indent [flat-tokens {:keys [*index] :as opts}]
  (loop [data []
         ignore-data []
         last-index @*index
         new-line? false]
    (if-let [[group _ :as token-data] (read-structured-token flat-tokens opts)]
      (cond
        (= :delimiter group)
        (recur
          data
          (conj ignore-data (vary-meta token-data assoc :action :remove))
          last-index
          new-line?)
        (whitespace? group)
        (recur
          data
          (conj ignore-data token-data)
          last-index
          (or new-line? (= group :newline-and-indent)))
        :else
        (if new-line?
          (recur
            (-> data
                (into ignore-data)
                (conj token-data))
            []
            @*index
            new-line?)
          (recur
            data
            (conj ignore-data token-data)
            last-index
            new-line?)))
      (do
        (vreset! *index last-index)
        data))))

(defn- read-coll-indent-mode [flat-tokens [_ delim :as token-data] {:keys [*index] :as opts}]
  (let [end-delim (delims delim)
        indent (-> token-data meta :indent)
        opts (assoc opts :min-indent indent)]
    (loop [data [token-data]
           whitespace-data []
           last-index @*index]
      (if-let [[group token :as token-data] (read-structured-token flat-tokens opts)]
        (cond
          (whitespace? group)
          (recur data (conj whitespace-data token-data) last-index)
          (< (-> token-data meta :indent) indent)
          (do
            (vreset! *index last-index)
            (-> data
                (insert-delim end-delim)
                wrap-coll))
          (= :delimiter group)
          (if (= token end-delim)
            (let [tokens-to-move (read-next-tokens-with-indent flat-tokens opts)]
              (if (seq tokens-to-move)
                (-> data
                    (into whitespace-data)
                    (remove-token token-data)
                    (into tokens-to-move)
                    (insert-token token-data)
                    wrap-coll)
                (-> data
                    (into whitespace-data)
                    (conj token-data)
                    wrap-coll)))
            (recur
              (-> data
                  (into whitespace-data)
                  (remove-token token-data))
              []
              @*index))
          :else
          (recur
            (-> data
                (into whitespace-data)
                (conj token-data))
            []
            @*index))
        (do
          (vreset! *index last-index)
          (-> data
              (insert-delim end-delim)
              wrap-coll))))))

(defn- read-coll-paren-mode [flat-tokens [_ delim :as token-data] {:keys [indent-change *error] :as opts}]
  (let [end-delim (delims delim)
        min-indent (cond-> (-> token-data meta :indent)
                           indent-change
                           (+ indent-change))
        opts (dissoc opts :min-indent)]
    (loop [data [token-data]
           max-indent nil
           indent-change (or indent-change 0)]
      (if-let [[group token :as token-data] (read-structured-token flat-tokens
                                              (assoc opts :indent-change indent-change))]
        (cond
          (= :newline-and-indent group)
          (let [current-indent (-> token-data meta :indent)
                indent-change (cond
                                (< current-indent min-indent)
                                (- min-indent current-indent)
                                (and max-indent (> current-indent max-indent))
                                (- max-indent current-indent)
                                :else
                                0)]
            (recur
              (cond
                (pos? indent-change)
                (-> data
                    (conj token-data)
                    (conj (vary-meta [:whitespace (str/join (repeat indent-change " "))]
                            assoc :action :insert)))
                (neg? indent-change)
                (-> data
                    (conj (update token-data 1 subs 0 (+ (count token) indent-change)))
                    (conj (vary-meta [:whitespace (str/join (repeat (* -1 indent-change) " "))]
                            assoc :action :remove)))
                :else
                (conj data token-data))
              max-indent
              indent-change))
          (= :collection group)
          (recur
            (conj data token-data)
            (cond-> (-> token-data meta :indent dec)
                    max-indent
                    (min max-indent))
            indent-change)
          (= :delimiter group)
          (cond-> (wrap-coll (conj data token-data))
                  (not= token end-delim)
                  (vary-meta assoc :error-message (vreset! *error "Unmatched delimiter")))
          :else
          (recur (conj data token-data) max-indent indent-change))
        (vary-meta (wrap-coll data)
          assoc :error-message (vreset! *error "EOF while reading"))))))

(defn- read-coll [flat-tokens [_ delim :as token-data] {:keys [*index *error] :as opts}]
  (let [end-delim (delims delim)]
    (loop [data [token-data]]
      (if-let [[group token :as token-data] (read-structured-token flat-tokens opts)]
        (if (= :delimiter group)
          (cond-> (wrap-coll (conj data token-data))
                  (not= token end-delim)
                  (vary-meta assoc :error-message (vreset! *error "Unmatched delimiter")))
          (recur (conj data token-data)))
        (vary-meta (wrap-coll data)
          assoc :error-message (vreset! *error "EOF while reading"))))))

(defn- read-structured-token [flat-tokens {:keys [*index mode min-indent] :as opts}]
  (when-let [[group token :as token-data] (get flat-tokens (vswap! *index inc))]
    (when (or (nil? min-indent)
              (whitespace? group)
              (-> token-data meta :indent (>= min-indent)))
      (if (and (= :delimiter group)
               (open-delims token))
        (case mode
          :indent (read-coll-indent-mode flat-tokens token-data opts)
          :paren (read-coll-paren-mode flat-tokens token-data opts)
          :smart (let [{:keys [cursor-line cursor-column]} opts
                       {:keys [line column]} (meta token-data)]
                   (if (or (< line cursor-line)
                           (and (= line cursor-line)
                                (< column cursor-column)))
                     (read-coll-indent-mode flat-tokens token-data opts)
                     (read-coll-paren-mode flat-tokens token-data opts)))
          nil (read-coll flat-tokens token-data opts))
        token-data))))

(defn- read-useful-token [flat-tokens {:keys [mode *error] :as opts}]
  (when-let [[_ token :as token-data] (read-structured-token flat-tokens opts)]
    (cond
      (close-delims token)
      (if (#{:indent :smart} mode)
        (vary-meta token-data assoc :action :remove)
        (vary-meta token-data assoc :error-message (vreset! *error "Unmatched delimiter")))
      :else
      token-data)))

(defn parse
  ([s]
   (parse s {}))
  ([s opts]
   (when (and (= :smart (:mode opts))
              (or (nil? (:cursor-line opts))
                  (nil? (:cursor-column opts))))
     (throw (ex-info "Smart mode requires :cursor-line and :cursor-column" {})))
   (let [matcher (->regex s)
         *error? (volatile! false)
         *line (volatile! 0)
         *column (volatile! 0)
         *indent (volatile! 0)
         tokens (loop [tokens (transient [])]
                  (if (find matcher)
                    (recur (conj! tokens (read-token matcher *error? *line *column *indent)))
                    (persistent! tokens)))
         opts (assoc opts
                :*error (volatile! nil)
                :*index (volatile! -1))
         opts (cond-> opts @*error? (dissoc :mode))]
     (loop [structured-tokens []]
       (if-let [token-data (read-useful-token tokens opts)]
         (recur (conj structured-tokens token-data))
         (let [{:keys [mode *error]} opts]
           (vary-meta structured-tokens
             assoc :mode mode :error? (some? @*error))))))))

(defn- node-iter [node-fn nodes node disable-parinfer?]
  (if (= (first node) :collection)
    (->> (reduce
           (fn [v child]
             (node-iter node-fn v child disable-parinfer?))
           []
           (rest node))
         (into (with-meta [:collection] (meta node)))
         node-fn
         (conj nodes))
    (if (if disable-parinfer?
          (-> node meta :action (= :insert))
          (-> node meta :action (= :remove)))
      nodes
      (conj nodes (node-fn node)))))

(defn flatten
  ([parsed-code]
   (->> parsed-code
        (flatten #(-> % rest str/join))
        str/join))
  ([node-fn parsed-code]
   (let [m (meta parsed-code)
         disable-parinfer? (and (= :paren (:mode m))
                                (:error? m))]
     (reduce
       (fn [v code]
         (into v (node-iter node-fn [] code disable-parinfer?)))
       []
       parsed-code))))

(defn- diff-node [*line *column *diff parent-node node]
  (if (vector? node)
    (let [[type & children] node]
      (when (= type :newline-and-indent)
        (vswap! *line inc)
        (vreset! *column -1))
      (run! (partial diff-node *line *column *diff node) children))
    (let [line @*line
          column @*column]
      (vswap! *column + (count node))
      (when-let [action (:action (meta parent-node))]
        (vswap! *diff conj {:line line
                            :column column
                            :content node
                            :action action
                            :type (first parent-node)})
        (when (= action :remove)
          (vswap! *column - (count node)))))))

(defn diff [parsed-code]
  (let [*line (volatile! 0)
        *column (volatile! 0)
        *diff (volatile! [])
        m (meta parsed-code)
        disable-parinfer? (and (= :paren (:mode m))
                               (:error? m))]
    (when-not disable-parinfer?
      (run! (partial diff-node *line *column *diff nil) parsed-code))
    @*diff))

