(ns paren-salad.core
  (:require [clojure.string :as str])
  (:refer-clojure :exclude [find])
  #?(:clj (:import [java.util.regex Pattern Matcher])))

(def group-names [:newline-and-indent
                  :whitespace
                  :special-char
                  :delimiter
                  :string
                  :comment
                  :number
                  :symbol])

(def groups ["(\n[ ]*)"                    ;; newline-and-indent
             "([\\s,]+)"                   ;; whitespace
             "(~@|['`~^@])"                ;; special-char
             "([\\[\\]{}()]|#\\{)"         ;; delimiter
             "(\"(?:\\\\.|[^\\\\\"])*\"?)" ;; string
             "(;.*)"                       ;; comment
             "(\\d+\\.?[a-zA-Z\\d]*)"      ;; number
             "([^\\s\\[\\]{}('\"`,;)]+)"]) ;; symbol

(def group-range (range 0 (count group-names)))

(def regex-str (str/join "|" groups))
(def regex (re-pattern regex-str))

(def open-delims #{"#{" "(" "[" "{"})
(def close-delims #{"}" ")" "]"})
(def delims {"#{" "}"
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

(defn read-token [matcher *line *column *indent]
  (let [token (group matcher 0)
        group (get group-names
                (some #(when (group matcher (inc %)) %) group-range)
                :whitespace)
        start-line @*line
        start-column @*column
        end-line (if (= group :newline-and-indent)
                   (vswap! *line inc)
                   @*line)
        end-column (if (= group :newline-and-indent)
                     (vreset! *column (dec (count token)))
                     (vswap! *column + (count token)))
        indent (cond
                 (open-delims token)
                 (vswap! *indent + (count token))
                 (= group :newline-and-indent)
                 (vreset! *indent (dec (count token)))
                 :else
                 @*indent)
        token-data [(if (and (= group :symbol)
                             (str/starts-with? token ":"))
                      :keyword
                      group)
                    token]
        token-data (vary-meta token-data assoc
                     :start-line start-line
                     :start-column start-column
                     :end-line end-line
                     :end-column end-column
                     :indent indent)]
    token-data))

(defn wrap-coll [data]
  (into [:collection] data))

(defn read-coll [flat-tokens [_ delim :as token-data] *index {:keys [parinfer] :as opts}]
  (let [end-delim (delims delim)
        indent (-> token-data meta :indent)]
    (loop [data [token-data]]
      (if-let [[_ token :as token-data] (get flat-tokens (vswap! *index inc))]
        (cond
          (and (= :indent parinfer)
               (< (-> token-data meta :indent) indent))
          (do
            (vswap! *index dec) ;; read token again later
            (wrap-coll (conj data [:delimiter end-delim])))
          (= token end-delim)
          (wrap-coll (conj data token-data))
          (close-delims token)
          (if (= :indent parinfer)
            (wrap-coll (conj data [:delimiter end-delim]))
            (vary-meta (wrap-coll (conj data token-data))
              assoc :error-message "Unmatched delimiter"))
          :else
          (recur (conj data token-data)))
        (if (= :indent parinfer)
          (wrap-coll (conj data [:delimiter end-delim]))
          (vary-meta (wrap-coll data)
            assoc :error-message "EOF while reading"))))))

(defn read-structured-token [flat-tokens *index opts]
  (when-let [[_ token :as token-data] (get flat-tokens (vswap! *index inc))]
    (cond
      (open-delims token)
      (read-coll flat-tokens token-data *index opts)
      (close-delims token)
      (when-not (= :indent (:parinfer opts))
        (vary-meta token-data assoc :error-message "Unmatched delimiter"))
      :else
      token-data)))

(defn parse
  ([s]
   (parse s {}))
  ([s opts]
   (let [matcher (->regex s)
         *line (volatile! 0)
         *column (volatile! 0)
         *indent (volatile! 0)
         tokens (loop [tokens []]
                  (if (find matcher)
                    (recur (conj tokens (read-token matcher *line *column *indent)))
                    tokens))
         *index (volatile! -1)]
     (loop [structured-tokens []]
       (if-let [token (read-structured-token tokens *index opts)]
         (recur (conj structured-tokens token))
         structured-tokens)))))

