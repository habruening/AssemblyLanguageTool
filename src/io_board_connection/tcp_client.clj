(ns io-board-connection.tcp-client
  (:import [java.net Socket]
           [java.io PrintWriter BufferedReader InputStreamReader])
  (:require [misc.cljboost :refer :all]
            [clojure.core.async :as a :refer [>! <! >!! <!!]]))

;   Hardware Tracing
; ========================================================================
;                                           .
;     _________________                     .                  
;    /_______CPU_______\                    .
;    |||||||||||||||||||                    .
;    ___________________                    .     |----------------------|      |------------------|
;   |  Tracer Adapter   |------------------------>| proprietary software |----->| Coverage Results |
;    -------------------                    .     |----------------------|      |------------------|
;    |||||||||||||||||||                    .                                   
;   _____________________________________   .                       |----------|    
;  |            Development Board        |  .                       | Assembly |         |----------|
;   -------------------------------------   .    |----------|       |  Code    |<--------| Embedded |           .
;                                    ^---------- | Firmware |       |----------|         | Software |
;                                           .    |  Loader  |<------|  Binary  |<--------|   Code   |
;                                           .    |----------|       |----------|         |----------|
; 
;   Software Tracing
; ========================================================================
;                                           .    |------------------Clojure---------------|             
;                                           .    |                                        |
;                              TCP Stream   .    ||------------|        |----------------||    |---------------|
;                                    ----------->|| TCP Client |------->| Trace Recorder |---->| Coverage File |
;    _________________               |      .    ||------------|        |----------------||    |---------------|
;   /_______CPU_______\              |      .    |---------------------------------^------|
;   __||||||||||||||||_______________|___   .                       |----------|   | 
;  |            Development Board        |  .                       | Assembly |----      |----------|
;   -------------------------------------   .    |----------|       |  Code    |<---------| Embedded |           .
;                                    ^-----------| Firmware |       |----------|          | Software |
;                                           .    |  Loader  |<------|  Binary  |<---------|   Code   |
;                                           .    |----------|       |----------|          |----------|

#_(defn- wait-for-ok [get-from-io-board]
  (while (if-let [from-io-board (get-from-io-board)]
           (not (= from-io-board "OK")))))

(comment 
  (wait-for-ok #(read-line)))

(defn- receive [get-from-io-board]
  (try
    (let [from-io-board (clojure.string/trim (get-from-io-board))]
      (cond (not from-io-board)
            [:terminated :connection-error]
            (clojure.string/blank? from-io-board)
            [:running nil]
            (= from-io-board "STARTED")
            [:running nil]
            (= from-io-board "DONE")
            [:terminated :tracing-done]
            :else
            (let [[from to to-be-nil] (clojure.string/split from-io-board #" ")]
              (println from-io-board from to to-be-nil)
              (cond (and from to (nil? to-be-nil))
                    [:running [(parse-int (str "0x" from)) (parse-int (str "0x" to))]]
                    (and from (nil? to))
                    [:running [(parse-int (str "0x" from)) (parse-int (str "0x" from))]]
                    :else
                    [:terminated :connection-error]))))
    (catch Exception e
      [:terminated :connection-error])))

(comment
  (receive #(read-line)))

(defn connect-to-io-board [host port trace-start-addr trace-end-addr]
  (let [to-trace-recorder (a/chan)
        socket (Socket. host port)
        writer (PrintWriter. (.getOutputStream socket) true)
        reader (BufferedReader. (InputStreamReader. (.getInputStream socket)))]
    #_(wait-for-ok #(.readLine reader))
    (.println writer (str "TRACE FROM " trace-start-addr " TO " trace-end-addr))
    #_(wait-for-ok #(.readLine reader))
    (a/thread (loop [[connection-state received-data] (receive #(.readLine reader))]
                (if received-data (>!! to-trace-recorder received-data))
                (if (= connection-state :terminated)
                  (a/close! to-trace-recorder)
                  (recur (receive #(.readLine reader)))))
              (.close reader)
              (.close writer)
              (.close socket))
    to-trace-recorder))

(comment
  (def c (connect-to-io-board "127.0.0.1" 7777 234 567))

  (<!! c)
  
  (loop [from-tracer "Connection establised"]
    (when from-tracer
      (println from-tracer)
      (recur (<!! c)))))
