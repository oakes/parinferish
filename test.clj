(require
  '[clojure.data.json :as json]
  '[clojure.java.io :as io]
  '[paren-salsa.core :as ps])

(doseq [file-name ["indent-mode.json"]
        test-case (->> file-name
                       io/resource
                       slurp
                       json/read-str)
        :when (empty? (get test-case "options"))]
  (let [{:strs [text result]} test-case
        opts {:parinfer :indent}
        ps-data (ps/parse text opts)
        ps-result (ps/flatten ps-data)]
    (when-not (= (get result "text") ps-result)
      (println "Test failed")
      (println "Input:   " (pr-str text))
      (println "Output:  " (pr-str ps-result))
      (println "Expected:" (pr-str (get result "text")))
      (println)
      #_(System/exit 0))))
