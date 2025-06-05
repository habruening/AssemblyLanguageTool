(ns asslangtool.core
  (:gen-class)
  (:require [io-board-connection.tcp-client]
            [asslangtool.xdada-1-2-listingfile]
            [asslangtool.coverage]
            [clojure.core.async :as a :refer [<!!]]))

#_(defn -main [listing-file at-address entry-point ip-address port output-file]

    (def from-tracer (io-board-connection.tcp-client/connect-to-io-board
                      ip-address
                      (Integer/parseInt port)
                      (read-string at-address)
                      (read-string entry-point)))

    (def from-recorder (-> listing-file
                           asslangtool.xdada-1-2-listingfile/load-listing-file
                           asslangtool.assembly-listing/assembly-listing
                           (asslangtool.coverage/start-tracing 0xA00 0x0)
                           (asslangtool.coverage/live-tracing from-tracer)))

    (loop [coverage (<!! from-recorder)]
      (when coverage
        (spit output-file
              (asslangtool.coverage/coverage-report (slurp listing-file) coverage))
        (recur (<!! from-recorder)))))

(defn -main [ip-address port first-address last-address start-ip output-file]

  (let [from-tracer (io-board-connection.tcp-client/connect-to-io-board
                     ip-address
                     (Integer/parseInt port)
                     (read-string first-address)
                     (read-string last-address))
        [listing-file assembly-lines] (asslangtool.assembly-listing/listing-file-for-testing-purposes 0 (- (read-string last-address) (read-string first-address)))
        from-recorder (-> {:code assembly-lines}
                          (asslangtool.coverage/start-tracing (read-string first-address) (read-string start-ip))
                          (asslangtool.coverage/live-tracing from-tracer))]
    (loop [coverage (<!! from-recorder)]
      (when coverage 
        (spit output-file
              (asslangtool.coverage/coverage-report listing-file coverage))
        (recur (<!! from-recorder))))))

(comment
  (-main "127.0.0.1" "7777" "0x3000" "0x3020" "2" "test.lis")
  )
