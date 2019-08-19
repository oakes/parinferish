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

(declare read-token)

(defn wrap-coll [data]
  (into [:collection] data))

(defn read-coll [matcher [_ delim :as token-data] opts]
  (let [end-delim (delims delim)]
    (loop [data [token-data]]
      (if (find matcher)
        (let [[_ token :as token-data] (read-token matcher opts)]
          (cond
            (= token end-delim)
            (wrap-coll (conj data token-data))
            (close-delims token)
            (if (= :indent (:parinfer opts))
              (wrap-coll (conj data [:delimiter end-delim]))
              (vary-meta (wrap-coll (conj data token-data))
                assoc :error-message "Unmatched delimiter"))
            :else
            (recur (conj data token-data))))
        (if (= :indent (:parinfer opts))
          (wrap-coll (conj data [:delimiter end-delim]))
          (vary-meta (wrap-coll data)
            assoc :error-message "EOF while reading"))))))

(defn read-token [matcher {:keys [*line *column *indent] :as opts}]
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
    (if (and (= group :delimiter)
             (open-delims token))
      (read-coll matcher token-data opts)
      token-data)))

(defn parse
  ([s]
   (parse s {}))
  ([s opts]
   (let [matcher (->regex s)
         opts (assoc opts :*line (volatile! 0) :*column (volatile! 0) :*indent (volatile! 0))]
     (loop [data []]
       (if (find matcher)
         (let [token (read-token matcher opts)]
           (if (close-delims token)
             (if (= :indent (:parinfer opts))
               (recur data)
               (recur (conj data (vary-meta token assoc :error-message "Unmatched delimiter"))))
             (recur (conj data token))))
         data)))))

