(ns clj-puredata-examples.text-helpers
  (:require [clj-puredata.core :refer :all]
            [clj-puredata-examples.modified-write-patch :refer [filename]]))

(def top-margin 12)
(def left-indent 25)
(def label-indent 180)
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
  "create a node description. returns description and y-offset for next item."
  (fn [y-pos]
    [(pd [(filename node) {:x left-indent :y y-pos}]
         [:text {:x label-indent :y y-pos} "- " description])
     line-height]))

(defn gap [line-count]
  (fn [y-pos]
    [nil (* line-count line-height)]))

(defn line [text]
  (fn [y-pos]
    [(pd [:text {:x left-indent :y y-pos} text])
     line-height]))

(defn layout-lines [& line-fs]
  "given line-functions with signature `y-offset -> [node y-offset]`, realise all functions in order, while summing the y-offset. returns collected nodes."
  (loop [[f & rest-fs] line-fs
         y-pos top-margin
         nodes []]
    (if (nil? f)
      (filter some? nodes)
      (let [[node y-offset] (f y-pos)]
        (recur rest-fs
               (+ y-pos y-offset)
               (conj nodes node))))))

(defn doc-strings [& strings]
  (apply layout-lines
         (for [s strings]
           (fn [y-pos]
             [(pd [:text {:x left-indent :y y-pos} s])
              line-height]))))

