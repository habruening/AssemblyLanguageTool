(ns asslangtool.coverage
  (:require [asslangtool.assembly-listing]
            [asslangtool.xdada-1-2-listingfile]
            [misc.mstring :as mstr]))

(defn start-tracing [code at-address start-ip]
  {:code (update-vals (code :code) #(assoc % :coverage #{}))
   :->relative-ip #(- % at-address)
   :current-ip (- start-ip at-address)})

(defn trace-to-and-branch [tracer current-ip branch-to]
  (let [current-ip ((tracer :->relative-ip) current-ip)
        branch-to ((tracer :->relative-ip) branch-to)
        code (tracer :code)
        is-executed? (fn [addr]
                       (and (<= (tracer :current-ip) addr)
                            (< addr current-ip)))
        executed-addrs (filter is-executed? (keys code))
        set-executed (fn [cov-type code addr]
                       (update-in code [addr :coverage] conj cov-type))
        code (set-executed :branched code current-ip) 
        code (reduce (partial set-executed :executed) code executed-addrs)]
    (assoc tracer :code code :current-ip branch-to)))


(defn coverage-report [listing-file tracer]
  (reduce (fn [listing-file {checkspace :checkspace coverage :coverage mnemonic :mnemonic}]
            (str (subs listing-file 0 (checkspace :start))
                 (let [is-branch (mstr/starts-with? mnemonic "B")] ; this is still incorrect
                   (cond
                     (and (coverage :executed) (coverage :branched))
                     " |__ "
                     (and (coverage :executed) is-branch)
                     "*|.. "
                     (and (coverage :executed))
                     " |   "
                     (and (coverage :branched) is-branch)
                     "* __ "
                     (coverage :branched)
                     "  __ "
                     :else
                     "     "))
                 (subs listing-file (checkspace :end))))
          listing-file
          (-> tracer :code vals)))

(spit
 "example.LIS"
 (let [tracing
       (start-tracing
        (asslangtool.assembly-listing/assembly-listing
         (asslangtool.xdada-1-2-listingfile/load-listing-file "test/TestData/JCOBITCP_JCOBTCC.LIS"))
        0xA00
        0x0)
       tracing (trace-to-and-branch tracing 0xA1c 0xA28)
       tracing (trace-to-and-branch tracing 0xA44 0xA54)
       tracing (trace-to-and-branch tracing 0xA62 0xA42)
       tracing (trace-to-and-branch tracing 0xA4C 0xA62)]

   (coverage-report (slurp "test/TestData/JCOBITCP_JCOBTCC.LIS") tracing)))


(update-in {:a 1 :b {:cov #{:c :d}}} [:b :cov] #(into % :d))

(conj #{:a :b} :c)