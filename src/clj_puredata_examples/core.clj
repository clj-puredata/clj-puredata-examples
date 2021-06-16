(ns clj-puredata-examples.core
  (:require [clj-puredata.core :refer [startup]]
            [clj-puredata-examples.modified-write-patch :refer :all]
            [clj-puredata-examples.text-helpers :refer :all]
            [clj-puredata-examples.utilities :as utilities]
            [clj-puredata-examples.patchbay :as patchbay]
            [clj-puredata-examples.help :as help]
            ))

(utilities/write)
(patchbay/write)
(help/write)

(comment
  (startup)
  (load-patches "help")
  (load-patches "counter" "counter-help")
  (load-patches "patchbay-help")
  )
