(ns clj-puredata-examples.patchbay
  (:require [clojure.string :as string ]
            [clj-puredata.core
             :refer [color-file
                     color-runtime
                     hsl2rgb
                     inlet
                     outlet
                     other
                     pd]
             :as cljpd]
            [clj-puredata-examples.modified-write-patch :refer :all]
            [clj-puredata-examples.text-helpers :refer :all]))

(defonce ignorable-counter (atom 0))

(defn ignorable []
  (swap! ignorable-counter inc)
  (str "ignored" @ignorable-counter))

(defn- shift
  [n lst]
  (apply conj (vec (drop n lst)) (vec (take n lst))))

(defn- cnv-line-msg
  "generate a pd message for aligning numerous canvasses along a line."
  [canvas-count
   canvas-id-fn ; function that returns the target canvas receive symbols. (int) -> the receive symbol for a canvas
   from-x from-y to-x to-y ; coordinates
   ]
  (let [frac-x (/ (- to-x from-x) (dec canvas-count))
        frac-y (/ (- to-y from-y) (dec canvas-count))]
    [:msg
     (string/join
      " "
      (for [n (range canvas-count)]
        (str "; " (canvas-id-fn n) " pos "
             (int (+ from-x (* frac-x n))) " "
             (int (+ from-y (* frac-y n))))))]))

(defn- cnv-square-line-msg
  "generate 3 messages to generate a line.
  assumes to-x is greater than from-x.
  can handle to-y being smaller than from-y."
  ;; todo handle to-x being smaller than from-x
  [& {:keys [canvas-id-fn from-x from-y to-x to-y midpoint thickness]
      :or {midpoint (+ from-x (java.lang.Math/floor (* (- to-x from-x) (java.lang.Math/random))))
           thickness 4}}]
  (let [[from-x from-y to-x to-y midpoint thickness]
        (map int [from-x from-y to-x to-y midpoint thickness])]
    [:msg
     (str "; " (canvas-id-fn 0) " pos " from-x " " from-y)
     (str "; " (canvas-id-fn 0) " vis_size " (- midpoint from-x) " " thickness)
     (str "; " (canvas-id-fn 1) " pos " midpoint " " (min from-y to-y))
     (str "; " (canvas-id-fn 1) " vis_size " thickness " " (+ (java.lang.Math/abs (int (- to-y from-y))) thickness))
     (str "; " (canvas-id-fn 2) " pos " midpoint " " to-y)
     (str "; " (canvas-id-fn 2) " vis_size " (- to-x midpoint) " " thickness)
     ]))

(defn count-some
  "like `(range (count COLL))` but there's `nil` for each `nil` in the COLL.
  ```clojure
  (count-some ['a 'b nil 'c]) => [[0 0] [1 1] [nil 2] [3 2]]
  ```"
  [coll]
  (loop [l coll
         i 0
         n 0
         accum []]
    (if (empty? l)
      accum
      (recur (drop 1 l)
             (if (some? (first l)) (inc i) i)
             (inc n)
             (conj accum [(if (some? (first l)) n nil)
                          i])))))

(defn patchbay
  "the patchbay is a big signal router that connects every input to every output.
  each route can be toggled on/off individually and dynamically.
  the interface has a bang for each input on the left side, and a bang for each output on the right side.
  to toggle a connection, click the input bang, then the output bang.

  there are several symbols defined for send and receive:
  
  - each 'input'-bang is receiving `patchbay-in-bang-N`, where n is the number of the inlet.
  - each 'output'-bang is receiving `patchbay-out-bang-N`, where n is the number of the inlet
  - the patchbay receives on its `:receive-symbol` (default: `patchbay-rcv`) and sends
    on its `:send-symbol` (default: `patchbay-snd`).
  
  sending a `bang` to the patchbay will output the current state of all toggles.
  you can send the state to the patchbay receive to restore it.

  NOTE: the patch requires the `cljpd.toggle.pd` abstraction. "
  [& {:keys [in-lines
             out-lines
             patch-file
             send-symbol
             receive-symbol]
      :or {in-lines ["in-0" "in-1" nil "in-2"]
           out-lines ["out-0" nil "out-1" "out-2"]
           patch-file "patchbay.pd"
           send-symbol "patchbay-snd"
           receive-symbol "patchbay-rcv"}}]
  (let [in-lines-safe (filter some? in-lines)
        in-count (count in-lines-safe)
        out-lines-safe (filter some? out-lines)
        out-count (count out-lines-safe)
        in-range (range in-count)
        out-range (range out-count)
        in-range-nils (count-some in-lines) ; range with nils, for position offset/empty positions
        out-range-nils (count-some out-lines)
        base-message (into (vec (take (dec in-count) (repeat 0))) [1])
        bang-size 15
        gap-size 6
        bang-plus-gap (+ bang-size gap-size)
        out-bang-offset (int (/ bang-plus-gap 2))
        canvas-size 4
        canvas-res 3
        canvas-id (fn [in out n] (str "ui-cnv-" in \- out \- n))
        toggle-id (fn [in out] (str "toggle-" out \- in))
        in-bang-id (fn [index] (str "patchbay-in-bang-" (nth in-lines-safe index)))
        out-bang-id (fn [index] (str "patchbay-out-bang-" (nth out-lines-safe index)))
        canvas-lines (for [in in-range
                           out out-range
                           n (range canvas-res)]
                       [:cnv {:x 0 :y 0
                              :size 0 ;canvas-size
                              :width 0 ;canvas-size
                              :height 0 ;canvas-size
                              :bg-color (color-file (hsl2rgb (mod (* in 1/6) 1) 0.5 0.5)) ;(color-file 0 0 0)
                              :receive-symbol (canvas-id in out n)}])
        canvas-msgs (for [out out-range-nils
                          in in-range-nils
                          :let [[out-n out-i] out
                                [in-n in-i] in]
                          :when (and (some? out-n)
                                     (some? in-n))]
                      (conj ;; (cnv-line-msg canvas-res (partial canvas-id in-i out-i)
                            ;;               (+ 33 bang-size) (+ 32 (/ (- bang-size canvas-size) 2)
                            ;;                                   (* in-n (+ gap-size bang-size)))
                            ;;               (- 99 canvas-size) (+ 32 (/ (- bang-size canvas-size) 2)
                            ;;                                     (* out-n (+ gap-size bang-size))))
                       (cnv-square-line-msg :canvas-id-fn (partial canvas-id in-i out-i)
                                            :from-x (+ 33 bang-size)
                                            :from-y (+ 32 (/ (- bang-size canvas-size) 2)
                                                       (* in-n bang-plus-gap))
                                            :to-x 99
                                            :to-y (+ 32 (/ (- bang-size canvas-size) 2)
                                                     (* out-n bang-plus-gap)
                                                     out-bang-offset)
                                            :midpoint (+ (* (inc in-i) 4) (+ 33 bang-size))
                                            :thickness 2)
                            [:select 1 (other (toggle-id in-i out-i))]))
        canvas-resets (for [line out-range
                            n in-range]
                        [:msg [:select 0 (other (toggle-id n line))]
                         (string/join " " (for [i (range canvas-res)]
                                            (str "; " (canvas-id n line i) " pos 0 0"
                                                 "; " (canvas-id n line i) " vis_size 0 0")))])
        in-bangs (for [in in-range-nils
                       :let [[in-n in-i] in]
                       :when (some? in-n)]
                   (pd [:bng {:x 32 :y (+ 32 (* in-n (+ gap-size bang-size)))
                              :size bang-size
                              :label-text (nth in-lines in-n)
                              :label-x -32
                              :receive-symbol (in-bang-id in-i)
                              :send-symbol (ignorable)}]))
        out-bangs (for [out out-range-nils
                        :let [[out-n out-i] out]
                        :when (some? out-n)]
                    (pd [:bng {:x 100 :y (+ 32 (* out-n (+ gap-size bang-size)) out-bang-offset)
                               :size bang-size
                               :label-text (nth out-lines out-n)
                               :receive-symbol (out-bang-id out-i)
                               :send-symbol (ignorable)}]))
        in-messages (for [n in-range]
                      (-> [:msg (string/join \space (reverse (shift n base-message))) (nth in-bangs n)]
                          (inlet 1)))
        out-messages (for [n out-range]
                       (-> [:msg n (nth out-bangs n)]
                           (inlet 0)))
        main-command (into [:list] (into out-messages in-messages))
        router (pd (into [:route main-command] (vec out-range)))
        unpackers (for [n out-range]
                    (pd (into [:unpack (outlet router n)] (vec (take in-count (repeat \f))))))
        catches (for [in in-lines-safe]
                  (pd [:catch- (str "patchbay-in-" in)]))
        matrixes (for [out out-range]
                   (for [in in-range]
                     [:*- (nth catches in)
                      [(filename "toggle") {:name (toggle-id in out)}
                       (outlet (nth unpackers out) in)
                       (-> [:msg "0"
                            (other (str "unpack-trig-" in \- out))
                            (-> 'rcv-route
                                other
                                (outlet 1)
                                (inlet 0))]
                           (inlet 0))
                       (-> [:t {:name (str "unpack-trig-" in \- out)} \b \f
                            (outlet (other 'rcv-unpack)
                                    (+ in (* out in-count)))]
                           (outlet 1)
                           (inlet 1))]]))
        reset-bang [:msg (str "; " receive-symbol \space (string/join \space (repeat (* in-count out-count) \0)))
                    [:bng {:x 80 :y 0 :label-text "clear"}]]
        receive [:unpack {:name 'rcv-unpack} (string/join " " (repeat (* in-count out-count) \f))
                 [:route {:name 'rcv-route} "list" "bang" [:r receive-symbol]]]
        send [:s send-symbol (into [:pack (string/join " " (repeat (* in-count out-count) \f))]
                                   (for [out out-range
                                         in in-range]
                                     (other (toggle-id in out))))]
        outputs (for [n out-range
                      :let [out (nth out-lines-safe n)]]
                  [:throw- (str "patchbay-out-" out)
                   (into [:+-] (map #(inlet % 0) (nth matrixes n)))])
        patch-width (+ 100 16 32)
        patch-height (+ 32
                        out-bang-offset
                        (* bang-plus-gap (max (count in-lines)
                                              (count out-lines))))]
    (cljpd/write-patch-reload patch-file
                              {:graph-on-parent true
                               :width patch-width
                               :height patch-height
                               :view-width patch-width
                               :view-height patch-height}
                              in-bangs
                              out-bangs
                              reset-bang
                              canvas-lines
                              canvas-msgs
                              canvas-resets
                              receive
                              send
                              outputs)))

(defn- patchbay-help []
  (let [patchbay-filename (filename "helpfile-patchbay")]
   (patchbay :in-lines ["sin" nil "saw" "sqr"]
             :out-lines [nil "left" nil "right"]
             :patch-file patchbay-filename)
   (write-patch-reload "patchbay-help"
                       {:width 600 :height 400
                        ;; :graph-on-parent true
                        ;; :view-width 300 :view-height 300
                        }
                       (layout-lines
                        (paragraph "the patchbay is a big signal router that connects every input to every output. each route can be toggled on/off individually and dynamically. the interface has a bang for each input on the left side, and a bang for each output on the right side. to toggle a connection, click the input bang, then the output bang."
                                   4)
                        (paragraph "there are several symbols defined for send and receive:"
                                   1)
                        (paragraph "- each 'input'-bang is receiving `patchbay-in-bang-N`, where n is the number of the inlet."
                                   2)
                        (paragraph "- each 'output'-bang is receiving `patchbay-out-bang-N`, where n is the number of the inlet"
                                   2)
                        (paragraph "- the patchbay receives on its `:receive-symbol` (default: `patchbay-rcv`) and sends on its `:send-symbol` (default: `patchbay-snd`)."
                                   3)
                        (paragraph "sending a `bang` to the patchbay will output the current state of all toggles. you can send the state to the patchbay receive to restore it."
                                   2)
                        (paragraph "NOTE: the patch requires the `cljpd.toggle.pd` abstraction."
                                   1))
                       [patchbay-filename]
                       [:msg "; patchbay-rcv bang"]
                       [:msg "; patchbay-rcv 1 0 1 0 1 0"]
                       [:bng {:send-symbol "patchbay-in-bang-sin" :label-text "sin"}]
                       [:bng {:send-symbol "patchbay-out-bang-left" :label-text "left"}]
                       [:print [:r "patchbay-snd"]]
                       [:throw- "patchbay-in-sin" [:*- 0.3 [:osc- 440]]]
                       [:throw- "patchbay-in-saw" [:*- 0.3 [:phasor- 110]]]
                       [:throw- "patchbay-in-sqr" [:*- 0.3 [:expr- "if($v1 > 0.5, 1, -1)" [:phasor- 220]]]]
                       [:dac-
                        [:catch- "patchbay-out-left"]
                        [:catch- "patchbay-out-right"]])))

(defn write []
  (patchbay :in-lines ["in-1" nil "in-2" "in-3"]
            :out-lines [nil "out-1" nil "out-2"]
            :send-symbol (ignorable)
            :receive-symbol (ignorable)
            :patch-file (filename "patchbay"))
  (patchbay-help))
