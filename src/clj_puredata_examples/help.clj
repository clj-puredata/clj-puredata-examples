(ns clj-puredata-examples.help
  (:require [clj-puredata-examples.modified-write-patch :refer [write-patch-reload]]
            [clj-puredata-examples.text-helpers :refer :all]))

(defn write []
  (write-patch-reload
   "help"
   {:width 500 :height 800}
   (layout-lines
    (paragraph "The following are the example patches of clj-puredata, a Clojure library for generating PureData patches. You can find the source code for these files at https://github.com/clj-puredata/clj-puredata-examples - feel free to adapt them to your own needs." 3)
    (chapter-mark "PATCHBAY")
    (node-description "patchbay" "complex signal routing matrix with UI")
    (gap 8)
    (chapter-mark "UTILITIES")
    (node-description "counter" "increase output with every bang")
    (node-description "count-until" "count from 0 to n")
    (node-description "nth" "(deprecated) select nth item from a list")
    (node-description "shift" "shift a list by n places")
    (node-description "toggle" "flip output between 0 and 1")
    (paragraph "generated by https://github.com/clj-puredata/clj-puredata-examples" 1.5)
    (paragraph "version 0.1.0" 2))))
