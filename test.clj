(require
  '[clojure.string :as str]
  '[clojure.java.io :as io]
  '[parinferish.core :as ps])

(doseq [mode [:indent :paren :smart]
        [in out] (-> (str (name mode) "-mode.md")
                     io/resource
                     slurp
                     (str/replace #"\n.*\^ error:.*" "")
                     (str/split #"```")
                     (->> (filter #(or (str/starts-with? % "in\n")
                                       (str/starts-with? % "out\n")))
                          (partition 2)
                          (map (fn [[in out :as pair]]
                                 (when-not (str/starts-with? in "in")
                                   (throw (ex-info (str "Expected \"in\":" \newline in) {})))
                                 (when-not (str/starts-with? out "out")
                                   (throw (ex-info (str "Expected \"out\":" \newline out) {})))
                                 (mapv #(subs % (inc (str/index-of % "\n")))
                                       pair)))))]
  (let [opts {:mode mode}
        res (ps/flatten (ps/parse in opts))]
    (when-not (= res out)
      (println "Test failed")
      (println "Options: " opts)
      (println "Input:   " (pr-str in))
      (println "Output:  " (pr-str res))
      (println "Expected:" (pr-str out))
      (println)
      (System/exit 0))))
