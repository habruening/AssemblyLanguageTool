(ns io-board-connection.http-client
  (:import [java.net Socket]
           [java.io PrintWriter BufferedReader InputStreamReader])
  (:require [clojure.core.async :as a :refer [>! >!! <!!]]))

(defn- wait-for-ok [reader]
  (while (if-let [from-io-board (.readLine reader)]
           (not (= from-io-board "OK")))))

(defn- receive-and-enqueue [reader c]
  (loop [from-io-board (.readLine reader)]
    (cond (not from-io-board)
          (>!! c :connection-error)
          (= from-io-board "DONE")
          (>!! c :tracing-done)
          :else
          (let [[from to to-be-nil] (clojure.string/split from-io-board #"->")]
            (if (not (and from to (nil? to-be-nil)))
              (>!! c :invalid-data)
              (do (println from to)
                (>!! c [(read-string from) (read-string to)]) ; todo: (read-string 43) does not work
                (recur (.readLine reader))))))))

(defn connect-to-io-board [host port trace-start-addr trace-end-addr]
  (let [c (a/chan)
        socket (Socket. host port)
        writer (PrintWriter. (.getOutputStream socket) true)
        reader (BufferedReader. (InputStreamReader. (.getInputStream socket)))]
    (wait-for-ok reader)
    (.println writer (str "TRACE FROM " trace-start-addr " TO " trace-end-addr))
    (wait-for-ok reader)
    (a/go (receive-and-enqueue reader c)
          (a/close! c)
          (.close reader)
          (.close writer)
          (.close socket))
    c))

(comment
  (def c (connect-to-io-board "127.0.0.1" 12345 234 567))
  (<!! c))
