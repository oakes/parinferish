(require
  '[clojure.data.json :as json]
  '[clojure.java.io :as io]
  '[parinferish.core :as ps])

(doseq [file-name ["paren-mode.json"]
        test-case (->> file-name
                       io/resource
                       slurp
                       json/read-str)
        :when (empty? (get test-case "options"))]
  (let [{:strs [text result]} test-case
        opts {:parinfer :paren}
        ps-data (ps/parse text opts)
        ps-result (ps/flatten ps-data)]
    (when-not (= (get result "text") ps-result)
      (println "Test failed")
      (println "Input:   " (pr-str text))
      (println "Output:  " (pr-str ps-result))
      (println "Expected:" (pr-str (get result "text")))
      (println)
      #_(System/exit 0))))
