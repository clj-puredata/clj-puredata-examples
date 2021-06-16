(ns clj-puredata-examples.utilities
  (:require [clj-puredata.core :refer [other inlet outlet startup]]
            [clj-puredata-examples.modified-write-patch :refer :all]
            [clj-puredata-examples.text-helpers :refer :all]))


(defn- counter []
  (write-patch "counter"
               [:outlet [:f {:name 'f} 0 [:b [:inlet]] [:+ 1 (other 'f)]]]))

(defn- counter-help [] 
  (write-patch "counter-help"
               (doc-strings "increase output with every bang."
                            "start at zero.")
               [:print "counter output" [(filename "counter") [:msg "bang"]]]))

(defn- count-until []
  (write-patch "count-until"
               [:outlet [:f {:name 'f} 0
                         [:until [:f 0 (outlet [:t {:name 'trig} "b f" [:inlet]] 1)]]
                         [:+ 1 (other 'f)]
                         (inlet [:f 0 (outlet (other 'trig) 0)] 1)]]))

(defn- count-until-help []
  (write-patch "count-until-help"
               [:print [(filename "count-until")
                        [:msg 0]
                        (inlet [:msg 3] 0)
                        (inlet [:msg 5] 0)
                        (inlet [:msg 7] 0)]]))

;; select nth item from list.\n
;; inlet 0: item #\n
;; inlet 1: list\n
(defn- nth_ []
  (write-patch "nth"
               [:inlet {:x 0 :name 'left}] ; float inlet
               [:inlet {:x 100 :name 'right}] ; list inlet
               [:t {:name 'trig} "b f" (outlet [:moses 0 (other 'left)] 1)]
               [:outlet [:list "split" 1 (outlet [:list "split"
                                                  [:list (outlet (other 'trig) 0) (other 'right)]
                                                  (outlet (other 'trig) 1)] 1)]]))

(defn- nth_-help []
  (write-patch "nth-help"
               (doc-strings "deprecated! use 'list store' instead"
                            "select the nth item from a list."
                            "inlet 0: item #."
                            "inlet 1: list."
                            "ignores item # below 0 or above list size.")
               [:bng [:symbolatom [(filename "nth")
                                   [:floatatom {:lower-limit -3
                                                :upper-limit 13}]
                                   [:msg "a b c d e f g h i j" [:loadbang]]]]]))

;; shift list by n places.
(defn- shift []
  (write-patch-reload "shift"
                      [:t {:name 'left} \b \f [:inlet {:x 0}]]
                      [:list {:name 'list}
                       (other 'left)
                       (inlet [:inlet {:name 'right :x 100}] 1)]
                      [:mod {:name 'mod}
                       (outlet (other 'left) 1)
                       [:list "length" (other 'right)]]
                      [:list {:name 'splitter} "split"
                       (other 'list)
                       (other 'mod)]
                      [:outlet
                       [:list "prepend"
                        (outlet (other 'splitter) 0)
                        (outlet (other 'splitter) 1)]]))

(defn- shift-help []
  (write-patch "shift-help"
               (doc-strings "shift a list by n places."
                            "left inlet: number of places to shift"
                            "right inlet: the list to use")
               [:print
                [(filename "shift")
                 [:floatatom {:lower-limit -5 :upper-limit 10}]
                 [:msg "1 2 3 4 5" [:loadbang]]]]))

(defn- toggle []
  (write-patch-reload "toggle"
                      [:outlet [:f {:name 'f} 0 [:!= [:inlet {:x 0}] (other 'f) (inlet (other 'setter) 1)]
                                [:inlet {:name 'setter :x 100}]]]))

(defn- toggle-help []
  (write-patch-reload "toggle-help"
                      (doc-strings "toggles output between 0 and 1."
                                   "left inlet toggles output on 1, keeps same output on 0."
                                   "right inlet sets internal value (1 or 0).")
                      [:floatatom [(filename "toggle") [:msg 1] (inlet [:msg 0] 0) (inlet [:msg 1] 1) (inlet [:msg 0] 1)]]))

(defn write []
  (counter)     (counter-help)
  (count-until) (count-until-help)
  (nth_)        (nth_-help)
  (shift)       (shift-help)
  (toggle)      (toggle-help))

