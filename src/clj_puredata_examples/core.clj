(ns clj-puredata-examples.core
  (:require [clj-puredata.core :refer :all]))

(def top-margin 12)
(def left-indent 25)
(def label-indent 125)
(def line-height 25)

(defn paragraph [text line-count]
  (fn [y-pos]
    [(pd [:text {:x left-indent :y y-pos} text])
     (* line-height line-count)]))

(defn chapter-mark [title]
  "creates a chapter mark. returns chapter mark and y-offset for next item."
  (fn [y-pos]
    [(pd [:text {:x left-indent :y (float (+ y-pos (/ line-height 2)))} "---------------- " title " ----------------"])
     (* line-height 2)]))

(defn node-description [node description]
  "create a node descriptsion. returns description and y-offset for next item."
  (fn [y-pos]
    [(pd [node {:x left-indent :y y-pos}]
         [:text {:x label-indent :y y-pos} "- " description])
     line-height]))

(defn layout-lines [& line-fs]
  "given line-functions with signature `y-offset -> [node y-offset]`, realise all functions in order, while summing the y-offset. returns collected nodes."
  (loop [[f & rest-fs] line-fs
         y-pos top-margin
         nodes []]
    (if (nil? f)
      nodes
      (let [[node y-offset] (f y-pos)]
        (recur rest-fs
               (+ y-pos y-offset)
               (conj nodes node))))))

(defn write-list-of-objects []
  (write-patch-reload
   "help.pd"
   (layout-lines
    (paragraph "The following are the example files for clj-puredata, a Clojure library for generating PureData patches. You can find the source code for these files at https://github.com/clj-puredata/clj-puredata-examples/ - feel free to adapt them to your needs." 3)
    (chapter-mark "GENERAL")
    (node-description :+ "adds 2 numbers")
    (node-description :- "subtracts 2 numbers")
    (chapter-mark "MISC"))
   ))

(write-list-of-objects)

(comment
  (startup)
  (load-patches "help.pd")
  )


