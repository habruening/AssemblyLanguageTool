(ns asslangtool.xdada-1-2-listingfile
  (:require [misc.mstring :as mstr]
            [misc.cljboost :refer :all]
            [clojure.tools.logging :refer [info warn error]]))

(defn page-with-correct-header? [lines]
  " A correct page header looks like this:
JCOBTCC_BIT_Test_Controller                                     28-Apr-2017 14:57:43    XD Ada V1.2A-33                     Page   2
01                                                              28-Apr-2017 14:54:46    JCOBITCP_JCOBTCC.ADA;1                   (1)"
  (cond (mstr/empty? lines) :correct
        (< (mstr/count lines) 4) :too-few-lines
        (not (mstr/empty? (lines 0))) :line-0-not-empty
        (not (= (mstr/count (lines 1)) 132)) :line-1-wrong-length
        (not (= (mstr/count (lines 2)) 132)) :line-2-wrong-length
        (= (mstr/first (lines 1)) \space) :line-1-starts-with-space
        (not (mstr/starts-with? (mstr/subs (lines 1) 124) "Page ")) :line-1-not-ending-with-page
        (= (mstr/first (lines 2)) \space) :line-2-starts-with-space
        (not (= (mstr/last (lines 2)) \))) :line-2-does-not-ending-with-closing-paren
        (not (mstr/empty? (lines 3))) :line-3-not-empty
        :else :correct))

(defn remove-page-headers [lines]
  (loop [contents []
         page-header {:lst-page-no 0 :page-header ""}
         [line-0 line-1 line-2 line-3 & more-lines] (seq lines)]
    (cond (= (page-with-correct-header? [line-0 line-1 line-2 line-3]) :correct)
          (let [form-feed-page-no (:page-no line-1)
                lst-page-no (inc (page-header :lst-page-no))]
            (if (and form-feed-page-no (not (= form-feed-page-no lst-page-no)))
              (warn "The page numbers do not match. Found the page header number" lst-page-no ". But when counting the form feeds"
                    "it is page number" form-feed-page-no ". Has the file been changed in a text editor that removed the form feed"
                    "symbols? Has the command `dos2unix` been used. We can continue here. But it looks like you are not analysing"
                    "the original listing files from the compiler."))
            (recur contents
                   (assoc page-header :page-header [line-1 line-2] :lst-page-no lst-page-no)
                   more-lines))
          line-0
          (recur (conj contents (mstr/add-context line-0 page-header))
                 page-header
                 (conj more-lines line-3 line-2 line-1))
          :else contents)))

(defn reconstruct-lines
  ([[line & more-lines] max-line-length]
   (cond (not line)
         nil
         (mstr/empty? line)
         (reconstruct-lines more-lines max-line-length)
         (<= max-line-length (mstr/count line))
         (reconstruct-lines [line] more-lines max-line-length)
         :else
         (lazy-seq (cons line (reconstruct-lines more-lines max-line-length)))))
  ([lines-before [line & more-lines] max-line-length]
   (cond (not line)
         [(apply mstr/str lines-before)]
         (mstr/empty? line)
         (lazy-seq (cons (apply mstr/str lines-before) (reconstruct-lines more-lines max-line-length)))
         (mstr/starts-with? line " ")
         (lazy-seq (cons (apply mstr/str lines-before) (reconstruct-lines (lazy-cat [line] more-lines) max-line-length)))
         (< (count line) max-line-length)
         (lazy-seq (cons (apply mstr/str (conj lines-before line)) (reconstruct-lines more-lines max-line-length)))
         :else
         (reconstruct-lines (conj lines-before line) more-lines max-line-length))))

(defn load-listing-file [file-name]
  (-> file-name
      slurp
      mstr/split-pages-and-lines
      remove-page-headers
      (reconstruct-lines 132)))

    
