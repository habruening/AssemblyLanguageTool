(ns misc.cljboost-test
  (:require [clojure.test :refer :all]
            [misc.cljboost :refer :all]
            [misc.testboost :refer :all]))

(testing "`as-max-function`"
  (protocol
   ((as-max-function <) 3 4 2 4 4 5 3) => 5
   ((as-min-function <) 3 4 2 4 4 5 3) => 2)


  (let [longer #(< (count %1) (count %2))
        longest (as-max-function longer)]
    (protocol
     (longest "A" "B" "CD" "E") => "CD"))

  (let [longer #(< (count %1) (count %2))
        longest (as-min-function longer)]
    (protocol
     (longest "AB" "C" "DE") => "C")))