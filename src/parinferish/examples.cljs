(ns parinferish.examples
  (:require [parinferish.core])
  (:require-macros [dynadoc.example :refer [defexample defexamples]]))

(defexample parinferish.core/parse
  (parse "(+ 1 2)"))

(defexamples parinferish.core/flatten
  ["Run indent mode"
   (parinferish.core/flatten (parinferish.core/parse "(foo a" {:mode :indent}))]
  ["Run paren mode"
   (parinferish.core/flatten (parinferish.core/parse "(foo\na)" {:mode :paren}))]
  ["Run smart mode"
   (parinferish.core/flatten (parinferish.core/parse "([1\n 2\n 3]" {:mode :smart :cursor-line 0 :cursor-column 1}))])

(defexample parinferish.core/diff
  (parinferish.core/diff (parinferish.core/parse "([1\n 2\n 3]" {:mode :smart :cursor-line 0 :cursor-column 1})))

