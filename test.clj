(require
  '[clojure.data.json :as json]
  '[clojure.java.io :as io]
  '[parinferish.core :as ps])

(doseq [mode [:indent :paren :smart]
        test-case (->> (str (name mode) "-mode.json")
                       io/resource
                       slurp
                       json/read-str)
        :let [{:strs [text result options]} test-case
              cursor-line (get options "cursorLine" 0)
              cursor-column (get options "cursorX" 0)]]
  (let [opts {:mode mode
              :cursor-line cursor-line
              :cursor-column cursor-column}
        ps-data (ps/parse text opts)
        ps-result (ps/flatten ps-data)]
    (when-not (= (get result "text") ps-result)
      (println "Test failed")
      (println "Options: " opts)
      (println "Input:   " (pr-str text))
      (println "Output:  " (pr-str ps-result))
      (println "Expected:" (pr-str (get result "text")))
      (println)
      #_(System/exit 0))))
