(require
  '[clojure.data.json :as json]
  '[clojure.java.io :as io]
  '[paren-salad.core :as ps])

(doseq [file-name ["indent-mode.json"]
        test-case (->> file-name
                       io/resource
                       slurp
                       json/read-str)]
  (let [{:strs [text result options]} test-case
        opts {:parinfer :indent
              :cursor-line (get options "cursorLine")
              :cursor-column (get options "cursorX")}
        ps-data (ps/parse text opts)
        ps-result (ps/flatten ps-data)]
    (when-not (= (get result "text") ps-result)
      (println "Test failed:")
      (println (pr-str ps-data))
      (println "Options:" options)
      (println "Input:" (pr-str text))
      (println "Output:" (pr-str ps-result))
      (println "Expected:" (pr-str (get result "text")))
      (println)
      #_(System/exit 0))))
