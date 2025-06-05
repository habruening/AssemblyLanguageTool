(ns asslangtool.coverage
  (:require [asslangtool.assembly-listing :refer [assembly-listing]]
            [asslangtool.xdada-1-2-listingfile :refer [load-listing-file]]
            [misc.mstring :as mstr]
            [clojure.core.async :as a :refer [>!! <!!]]))

(defn start-tracing [code at-address start-ip] 
  {:code (update-vals (code :code) #(assoc % :coverage #{}))
   :->relative-ip #(- % at-address)
   :current-ip (+ at-address start-ip)})

(defn trace-to-and-branch [tracer current-ip branch-to] 
  (let [curr-relative-ip ((tracer :->relative-ip) (tracer :current-ip))
        next-relative-ip ((tracer :->relative-ip) current-ip)
        code (tracer :code)
        is-executed? (fn [addr]
                       (and (<= curr-relative-ip addr)
                            (< addr next-relative-ip)))
        executed-addrs (filter is-executed? (keys code))
        set-executed (fn [cov-type code addr] 
                       (update-in code [addr :coverage] conj cov-type))
        code (set-executed :taken code next-relative-ip)
        code (reduce (partial set-executed :executed) code executed-addrs)]
    (assoc tracer :code code :current-ip branch-to)))

(comment
  (def tracer-example
    (start-tracing {:code {0x16 {:instr "move"}
                           0x17 {:instr "add"}
                           0x19 {:instr "dec"}
                           0x20 {:instr "be"}
                           0x21 {:instr "add"}
                           0x23 {:instr "div"}}}
                   0xA00
                   0x17))

  (tracer-example :code))

(comment
  ((trace-to-and-branch tracer-example 0xA20 0xA23) :code)

  (trace-to-and-branch
   (start-tracing {:code {0x16 {} 0x17 {} 0x19 {} 0x20 {} 0x21 {} 0x23 {}}}
                  0xA00 0x17)
   0xA20 0xA23))

(defn coverage-report [listing-file tracer]
  (reduce (fn [listing-file [address {checkspace :checkspace coverage :coverage mnemonic :mnemonic}]]
            (str (subs listing-file 0 (checkspace :start))
                 (let [is-branch (mstr/starts-with? mnemonic "B")] ; todo
                   (or (and (= ((tracer :->relative-ip) (tracer :current-ip))
                               address) "->   ")
                       (and (coverage :executed) (coverage :taken) " |__ ")
                       (and (coverage :executed) is-branch " |.. ")
                       (and (coverage :executed) " |   ")
                       (and (coverage :taken) is-branch " .__ ")
                       (and (coverage :taken) "  __ ")
                       "     "))
                 (subs listing-file (checkspace :end))))
          listing-file
          (-> tracer :code seq)))


(comment
  (spit "JCOBITCP_JCOBTCC_coverage.LIS"
        (let [tracing (start-tracing
                       (assembly-listing
                        (load-listing-file "test/TestData/JCOBITCP_JCOBTCC.LIS"))
                       0xA00
                       0x0)
              tracing (trace-to-and-branch tracing 0xA1c 0xA28)
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
                  (cond (= trace-event :connection-error) (a/close! to-monitor)
                        (= trace-event :tracing-done) (do (a/close! to-monitor) recording)
                        :no-error
                        (recur (trace-to-and-branch recording (first trace-event) (second trace-event)))))))
    to-monitor))

(comment
  (def from-tracer (a/chan))

  (def trace-recorder (-> "test/TestData/JCOBITCP_JCOBTCC.LIS"
                          load-listing-file
                          assembly-listing
                          (start-tracing 0xA00 0x0)))

  (def to-monitor (live-tracing trace-recorder from-tracer))

  (>!! from-tracer [0xA12 0xA1E])
  (>!! from-tracer [0xA28 0xA0C])
  (>!! from-tracer [0xA28 0xA34])
  (>!! from-tracer [0xA74 0xA74])
  (>!! from-tracer :tracing-done)

  (a/go (loop [from-recorder (<!! to-monitor)]
          (when from-recorder
            (spit "JCOBITCP_JCOBTCC_coverage.LIS"
                  (coverage-report (slurp "test/TestData/JCOBITCP_JCOBTCC.LIS") from-recorder))
            (recur (<!! to-monitor))))))



