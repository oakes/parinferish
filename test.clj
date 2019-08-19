(require
  '[clojure.data.json :as json]
  '[clojure.java.io :as io]
  '[paren-salad.core :as ps])

(doseq [file-name ["indent-mode.json"]
        test-case (->> file-name
                       io/resource
                       slurp
                       json/read-str)]
  (let [{:strs [text result]} test-case
        ps-result (-> text (ps/parse {:parinfer :indent}) ps/flatten)]
    (when-not (= (get result "text") ps-result)
      (println "Test failed:")
      (println "Input:" (pr-str text))
      (println "Output:" (pr-str ps-result))
      (println "Expected:" (pr-str (get result "text")))
      (println))))
