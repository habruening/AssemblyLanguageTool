(ns asslangtool.coverage
  (:require [asslangtool.assembly-listing :refer [assembly-listing]]
            [asslangtool.xdada-1-2-listingfile :refer [load-listing-file]]
            [misc.mstring :as mstr]
            [clojure.core.async :as a :refer [>!! <!!]]))

(defn address-in-range [start-addr end-addr addr]
  (and (<= start-addr addr)
       (< addr end-addr)))

(defn relative-addr [start-address addr]
  (- addr start-address))

(defn start-tracing [code start-address]
  {:code (update-vals (code :code) #(assoc % :coverage #{}))
   :start-address start-address
   :last-address (+ start-address (apply max (keys (code :code))))
   :current-ip :outside})

(defn trace-to-and-branch [tracer current-ip branch-to]
  (let [outside? (= (tracer :current-ip) :outside)
        into? (address-in-range (tracer :start-address) (tracer :last-address) branch-to)
        back? (and outside? into?)]
    (cond
      back?
      (assoc tracer :current-ip branch-to)
      (not outside?)
      (let [curr-relative-ip (relative-addr (tracer :start-address) (tracer :current-ip))
            next-relative-ip (relative-addr (tracer :start-address) current-ip)
            code (tracer :code)
            is-executed? (partial address-in-range curr-relative-ip next-relative-ip)
            executed-addrs (filter is-executed? (keys code))
            set-executed (fn [cov-type code addr]
                           (update-in code [addr :coverage] conj cov-type))
            code (set-executed :taken code next-relative-ip)
            code (reduce (partial set-executed :executed) code executed-addrs)]
        (assoc tracer :code code :current-ip (if into? branch-to :outside)))
      :still-outside
      tracer)))

(comment
  (def tracer-example
    (start-tracing {:code {0x16 {:instr "move"}
                           0x17 {:instr "add"}
                           0x19 {:instr "dec"}
                           0x20 {:instr "be"}
                           0x21 {:instr "add"}
                           0x23 {:instr "div"}}}
                   0xA00))

  (tracer-example :code))

(comment
  (trace-to-and-branch tracer-example 0x0 0xA19) 

  (-> {:code {0x16 {} 0x17 {} 0x19 {} 0x20 {} 0x21 {} 0x23 {} 0x25 {} 0x28 {}}}
      (start-tracing 0xA00)
      (trace-to-and-branch 0x00 0x10)
      (trace-to-and-branch 0x10 0xA19)
      (trace-to-and-branch 0xA21 0xA30)
      (trace-to-and-branch 0x40 0xA25)
      (trace-to-and-branch 0xA25 0xAA))
  )
 
(defn coverage-report [listing-file tracer]
  (reduce (fn [listing-file [address {checkspace :checkspace coverage :coverage mnemonic :mnemonic}]]
            (str (subs listing-file 0 (checkspace :start))
                 (let [is-branch (mstr/starts-with? mnemonic "B")] ; todo
                   (or (and (not (= (tracer :current-ip) :outside))
                            (= (relative-addr (tracer :start-address) (tracer :current-ip))
                               address)
                            "->   ")
                       (and (coverage :executed)
                            (coverage :taken)
                            " |__ ")
                       (and (coverage :executed) is-branch
                            " |.. ")
                       (and (coverage :executed)
                            " |   ")
                       (and (coverage :taken) is-branch
                            " .__ ")
                       (and (coverage :taken)
                            "  __ ")
                       "     "))
                 (subs listing-file (checkspace :end))))
          listing-file
          (-> tracer :code seq)))


(comment
  (spit "JCOBITCP_JCOBTCC_coverage.LIS"
        (let [tracing (start-tracing
                       (assembly-listing
                        (load-listing-file "test/TestData/JCOBITCP_JCOBTCC.LIS"))
                       0xA00)
              tracing (trace-to-and-branch tracing 0x000 0xA1C)
              tracing (trace-to-and-branch tracing 0xA1C 0xA28)
              tracing (trace-to-and-branch tracing 0xA44 0xA54)
              tracing (trace-to-and-branch tracing 0xA62 0xA42)
              tracing (trace-to-and-branch tracing 0xA4C 0xA62)]
          (coverage-report (slurp "test/TestData/JCOBITCP_JCOBTCC.LIS") tracing))))

;                                   |----------------|
;                                   | Trace Recorder |<----------------------
;                                   |----------------|                       |
;                                      /|\        |                   |--------------|
;                                       |         |          ---------| Listing File |
;       |------------|              |----------------|      |         |--------------|            
;------>| TCP Client |------------->|  Live Tracing  |      |              ||
;       |------------| from tracer  |----------------|      |              || comparable
;                                                 |         |              ||    
;                                                \|/       \|/             ||
;                                              |----------------|     |---------------|
;                                              | Trace Monitor  |---->| Coverage File |
;                                              |----------------|     |---------------|

(defn live-tracing [trace-recorder from-tracer]
  (let [to-monitor (a/chan)]
    (a/thread (loop [recording trace-recorder]
                (>!! to-monitor recording)
                (let [trace-event (<!! from-tracer)]
                  (cond (= trace-event :connection-error)
                        (a/close! to-monitor)
                        (= trace-event :tracing-done)
                        (do (a/close! to-monitor) recording)
                        :no-error
                        (recur (trace-to-and-branch recording (first trace-event) (second trace-event)))))))
    to-monitor))

(comment
  (def from-tracer (a/chan))

  (def trace-recorder (-> "test/TestData/JCOBITCP_JCOBTCC.LIS"
                          load-listing-file
                          assembly-listing
                          (start-tracing 0xA00)))

  (spit "JCOBITCP_JCOBTCC_coverage.LIS"
        (coverage-report (slurp "test/TestData/JCOBITCP_JCOBTCC.LIS")
                         (trace-to-and-branch trace-recorder 0x000 0xA12)))

  (def to-monitor (live-tracing trace-recorder from-tracer))

  (>!! from-tracer [0x000 0xA12])
  (>!! from-tracer [0xA12 0xA1E])
  (>!! from-tracer [0xA28 0xA0C])
  (>!! from-tracer [0xA28 0xA34])
  (>!! from-tracer [0xA74 0xA74])
  (>!! from-tracer :tracing-done)

  (a/go (loop [from-recorder (<!! to-monitor)]
          (when from-recorder
            (spit "JCOBITCP_JCOBTCC_coverage.LIS"
                  (coverage-report (slurp "test/TestData/JCOBITCP_JCOBTCC.LIS") from-recorder))
            (recur (<!! to-monitor)))))
  )

