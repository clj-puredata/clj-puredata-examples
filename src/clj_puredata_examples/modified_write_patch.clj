(ns clj-puredata-examples.modified-write-patch
  (:require [clj-puredata.core :as cljpd]))

(def filename-prefix "cljpd.")
(def filename-postfix ".pd")

(defn filename [fname]
  (str filename-prefix
       fname
       filename-postfix))

(defn write-patch [fname options & rest]
  (apply cljpd/write-patch
         (filename fname)
         options
         rest))

(defn write-patch-reload [fname options & rest]
    (apply cljpd/write-patch-reload
         (filename fname)
         options
         rest))

(defn load-patches [& rest]
  (apply cljpd/load-patches (map filename rest)))
