(ns misc.cljboost-test
  (:use midje.sweet)
  (:require [misc.cljboost]))

(facts
 ((as-max-function <) 3 4 2 4 4 5 3) => 5
 ((as-min-function <) 3 4 2 4 4 5 3) => 2)

(fact
 (let [longer #(< (count %1) (count %2))
       longest (as-max-function longer)]
   (longest "A" "B" "CD" "E")) => "CD")

(fact
 (let [longer #(< (count %1) (count %2))
       longest (as-min-function longer)]
   (longest "AB" "C" "DE")) => "C")