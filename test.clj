(require
  '[clojure.string :as str]
  '[clojure.java.io :as io]
  '[parinferish.core :as ps])

(defn parse-cases [mode]
  (-> (str (name mode) "-mode.md")
      io/resource
      slurp
      (str/replace #"\n.*\^ error:.*" "")
      (str/split #"```")
      (->> (filter #(or (str/starts-with? % "in\n")
                        (str/starts-with? % "out\n")))
           (map str/trim)
           (partition 2)
           (map (fn [[in out :as pair]]
                  (when-not (str/starts-with? in "in")
                    (throw (ex-info (str "Expected \"in\":" \newline in) {})))
                  (when-not (str/starts-with? out "out")
                    (throw (ex-info (str "Expected \"out\":" \newline out) {})))
                  (let [[new-in new-out] (mapv (fn [s]
                                                 (subs s (inc (str/index-of s "\n"))))
                                           pair)
                        ret {:in new-in :out new-out :opts {:mode mode}}]
                    (if (= :smart mode)
                      (let [pipe-pos (or (str/index-of new-in "|")
                                         (throw (ex-info (str "No pipe found:" \newline new-in) {})))
                            until-pipe (subs new-in 0 pipe-pos)]
                        (update ret :opts assoc
                          :cursor-line (->> until-pipe
                                            (re-seq #"\n")
                                            count)
                          :cursor-column (-> until-pipe
                                             (subs (or (some-> (str/last-index-of until-pipe "\n") inc)
                                                       0))
                                             count)))
                      ret)))))))

(doseq [mode [:indent :paren :smart]]
  (println "Testing" (name mode) "mode")
  (let [*success-count (atom 0)]
    (doseq [{:keys [in out opts]} (parse-cases mode)]
      (let [in (str/replace in #"\|" "")
            out (str/replace out #"\|" "")
            res (ps/flatten (ps/parse in opts))]
        (if (= res out)
          (swap! *success-count inc)
          (do
            (println "Test failed")
            (println "Options: " opts)
            (println "Input:   " (pr-str in))
            (println "Output:  " (pr-str res))
            (println "Expected:" (pr-str out))
            (println)))))
    (println @*success-count "tests passed")))

