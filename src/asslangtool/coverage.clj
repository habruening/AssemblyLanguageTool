(ns asslangtool.coverage
  (:require [asslangtool.assembly-listing]
            [asslangtool.xdada-1-2-listingfile]
            [misc.mstring :as mstr]))

(defn start-tracing [code start-address]
  {:code (update-vals (code :code) #(assoc % :coverage #{}))
   :current-ip start-address})

(defn trace-to-and-branch [tracer to branch-to]
  {:code (reduce (fn [code addr]
                   (update code addr
                           (fn [{coverage :coverage :as instr}]
                             (assoc instr :coverage (conj coverage :executed)))))
                 (-> tracer :code)
                 (filter #(and (<= (tracer :current-ip) %)
                               (< % to))
                         (-> tracer :code keys)))
   :current-ip branch-to})

(defn coverage-report [listing-file tracer]
  (reduce (fn [listing-file line-of-code]
            (str (subs listing-file 0 (-> line-of-code :checkspace :start))
                 (if (-> line-of-code :coverage :executed)
                   "|  "
                   "   ") 
                 (subs listing-file (-> line-of-code :checkspace :end))))
          listing-file
          (-> tracer :code vals)))


(println
 (let [tracing
       (start-tracing
        (asslangtool.assembly-listing/assembly-listing
         (asslangtool.xdada-1-2-listingfile/load-listing-file "test/TestData/JCOBITCP_JCOBTCC.LIS"))
        0x0)
       tracing (trace-to-and-branch tracing 0x1c 0x28)
       tracing (trace-to-and-branch tracing 0x44 0x54)]
   
   (coverage-report (slurp "test/TestData/JCOBITCP_JCOBTCC.LIS") tracing)))


