(ns asslangtool.xdada-1-2-listingfile-test
  (:require [clojure.test :refer :all]
            [misc.testboost :refer :all]
            [asslangtool.xdada-1-2-listingfile :as lst]
            [misc.mstring :as mstr]))

(testing "`listing-file-page-with-correct-header?`"

  (testing "with empty file"
    (protocol
     (lst/page-with-correct-header? []) => :correct))

  (testing "with examples"
    (loop [[header-0 header-1 warning empty-line & more-lines] (clojure.string/split-lines (slurp "test/TestData/PageHeaderTests.txt"))]
      (protocol
       (lst/page-with-correct-header? [header-0 header-1]) -> name => warning)
      (if (seq more-lines)
        (recur more-lines))))

  (testing "with incorrect page size"
    (let [[header-0 header-1 &_] (clojure.string/split-lines (slurp "test/TestData/PageHeaderTests.txt"))]
      (protocol
       (lst/page-with-correct-header? [header-0]) => :too-few-lines
       (lst/page-with-correct-header? [header-0 header-1]) => :correct
       (lst/page-with-correct-header? (into [header-0 header-1] (repeat 59 ""))) => :correct))))

(def all-as-jstr #(map mstr/->JString %))

(testing "`remove-page-headers`"
  (testing "with example"
    (protocol
     (lst/remove-page-headers
      ["-1.1"
       "JCOBTCC_BIT_Test_Controller                                     28-Apr-2017 14:57:43    XD Ada V1.2A-33                     Page   2"
       "01                                                              28-Apr-2017 14:54:46    JCOBITCP_JCOBTCC.ADA;1                   (1)"
       ""
       "0.1"
       "0.2"
       "JCOBTCC_BIT_Test_Controll                                       28-Apr-2017 14:57:43    XD Ada V1.2A-33                     Page   2"
       "02                                                              28-Apr-2017 14:54:46    JCOBITCP_JCOBTCC.ADA;1                   (1)"
       ""
       "01.1"
       "01.2"
       "JCOBTCC_BIT_Test_Controll                                       28-Apr-2017 14:57:43    XD Ada V1.2A-33                     Page   2"
       "03                                                              28-Apr-2017 14:54:46    JCOBITCP_JCOBTCC.ADA;1                   (1)"
       ""
       "JCOBTCC_BIT_Test_Controll                                       28-Apr-2017 14:57:43    XD Ada V1.2A-33                     Page   2"
       "03                                                              28-Apr-2017 14:54:46    JCOBITCP_JCOBTCC.ADA;1                   (1)"
       ""
       "03.1"
       "JCOBTCC_BIT_Test_Controll                                       28-Apr-2017 14:57:43    XD Ada V1.2A-33                     Page   2"
       "04                                                              28-Apr-2017 14:54:46    JCOBITCP_JCOBTCC.ADA;1                   (1)"
       ""])
     ->> (map (juxt :lst-page-no mstr/->JString))
     => [[0 "-1.1"] [1 "0.1"] [1 "0.2"] [2 "01.1"] [2 "01.2"] [4 "03.1"]]))

  (testing "with real example"
    (let [pages (->> "test/TestData/JCOBITCP_JCOBTCC.LIS"
                     slurp
                     mstr/split-pages-and-lines
                     lst/remove-page-headers
                   ; We don't expect a warning here.
                     (group-by :lst-page-no)
                     vals
                     (mapv all-as-jstr))]
      (protocol
       (nth pages 0) => (clojure.string/split (slurp "test/TestData/JCOBITCP_JCOBTCC_expected_page_1.LIS") #"\r?\n" -1)
       (nth pages 1) => (nthrest (clojure.string/split (slurp "test/TestData/JCOBITCP_JCOBTCC_expected_page_2.LIS") #"\r?\n" -1) 3)
       (nth pages 2) => (nthrest (clojure.string/split (slurp "test/TestData/JCOBITCP_JCOBTCC_expected_page_3.LIS") #"\r?\n" -1) 3)
       (nth pages 3) => (nthrest (clojure.string/split (slurp "test/TestData/JCOBITCP_JCOBTCC_expected_page_4.LIS") #"\r?\n" -1) 3)
       (nth pages 4) => (nthrest (clojure.string/split (slurp "test/TestData/JCOBITCP_JCOBTCC_expected_page_5.LIS") #"\r?\n" -1) 3)
       (nth pages 5) => (nthrest (clojure.string/split (slurp "test/TestData/JCOBITCP_JCOBTCC_expected_page_6.LIS") #"\r?\n" -1) 3)
       (nth pages 6) => (nthrest (clojure.string/split (slurp "test/TestData/JCOBITCP_JCOBTCC_expected_page_7.LIS") #"\r?\n" -1) 3)))))


     (testing "`remove-undesired-line-breaks"
       (protocol
        (lst/reconstruct-lines [""] 3) =fn=> all-as-jstr []
        (lst/reconstruct-lines ["" ""] 3) =fn=> all-as-jstr []
        (lst/reconstruct-lines ["" "" ""] 3) =fn=> all-as-jstr []
        (lst/reconstruct-lines ["1" "" ""] 3) =fn=> all-as-jstr ["1"]
        (lst/reconstruct-lines ["" "1" ""] 3) =fn=> all-as-jstr ["1"]
        (lst/reconstruct-lines ["" "" "1"] 3) =fn=> all-as-jstr ["1"]
        (lst/reconstruct-lines ["1"] 3) =fn=> all-as-jstr ["1"]
        (lst/reconstruct-lines ["1 2"] 3) =fn=> all-as-jstr ["1 2"]
        (lst/reconstruct-lines ["1 2" " 3"] 3) =fn=> all-as-jstr ["1 2", " 3"]
        (lst/reconstruct-lines ["1 2" " 34"] 3) =fn=> all-as-jstr ["1 2", " 34"]
        (lst/reconstruct-lines ["1 2" " 34" " 5"] 3) =fn=> all-as-jstr ["1 2", " 34", " 5"]
        (lst/reconstruct-lines ["1 2" "" " 3"] 3) =fn=> all-as-jstr ["1 2", " 3"]
        (lst/reconstruct-lines ["1 2" "3"] 3) =fn=> all-as-jstr ["1 23"]
        (lst/reconstruct-lines ["1  " "2"] 3) =fn=> all-as-jstr ["1  2"]
        (lst/reconstruct-lines [" " "1"] 3) =fn=> all-as-jstr [" ", "1"]
        (lst/reconstruct-lines ["  " "1"] 3) =fn=> all-as-jstr ["  ", "1"]
        (lst/reconstruct-lines ["1 2 "] 3) =fn=> all-as-jstr ["1 2 "]
        (lst/reconstruct-lines ["1 2 " " 3"] 3) =fn=> all-as-jstr ["1 2 ", " 3"]
        (lst/reconstruct-lines ["1 2 " "3"] 3) =fn=> all-as-jstr ["1 2 3"]
        (lst/reconstruct-lines ["1   " "2"] 3) =fn=> all-as-jstr ["1   2"]
        (lst/reconstruct-lines [" " "1"] 3) =fn=> all-as-jstr [" ", "1"]
        (lst/reconstruct-lines ["  " "1"] 3) =fn=> all-as-jstr ["  ", "1"]
        (lst/reconstruct-lines ["1 2" "3 4" "5"] 3) =fn=> all-as-jstr ["1 23 45"]
        (lst/reconstruct-lines ["1 2" "3 4" "5 6"] 3) =fn=> all-as-jstr ["1 23 45 6"]
        (lst/reconstruct-lines ["1 2" "3 4" "5 6" " 7"] 3) =fn=> all-as-jstr ["1 23 45 6", " 7"]
        (lst/reconstruct-lines ["1 2" "3 4" " "] 3) =fn=> all-as-jstr ["1 23 4", " "]
        (lst/reconstruct-lines ["1 2" "3 4" "5" ""] 3) =fn=> all-as-jstr ["1 23 45"]
        (lst/reconstruct-lines ["1 2" "3 4" "5 6" ""] 3) =fn=> all-as-jstr ["1 23 45 6"]
        (lst/reconstruct-lines ["1 2" "3 4" "5 6" " 7" ""] 3) =fn=> all-as-jstr ["1 23 45 6", " 7"]
        (lst/reconstruct-lines ["1 2" "3 4" " " ""] 3) =fn=> all-as-jstr ["1 23 4", " "]))

     (testing "`load-listing-file`"
       (let [test-results (lst/load-listing-file "test/TestData/JCOBITCP_JCOBTCC.LIS")
             expected-code (clojure.string/split-lines (slurp "test/TestData/JCOBITCP_JCOBTCC_expected_assembler_code.LIS"))
             get-line-no (fn [line]
                           (apply str (conj (if (:line-no line)
                                              [(inc (:line-no line)) ":"]
                                              [(-> line :parts first :line-no inc) ","
                                               (-> line :parts second :line-no inc) ":"])
                                            (mstr/->JString line))))]
         (doseq [[test-line expected-code] (take 77 (map vector test-results expected-code))]
           (if (not (is (= (get-line-no test-line)
                           expected-code)))
             (do (println (get-line-no test-line))
                 (println expected-code))))))


