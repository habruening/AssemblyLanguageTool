(ns misc.mstring-test
  (:refer-clojure :exclude [str subs count])
  (:require [clojure.test :refer :all]
            [misc.testboost :refer :all]
            [misc.mstring :as mstr :refer [JString TString VString MString ->JString add-context]]))

(testing "`dispatch-in-first-arg`"
  (protocol
   (mstr/dispatch-in-first-arg "A") => JString
   (mstr/dispatch-in-first-arg "A" 3) => JString
   (mstr/dispatch-in-first-arg {:super 0 :start 0 :end 0}) => TString
   (mstr/dispatch-in-first-arg {:super 0 :start 0 :end 0} 3) => TString
   (mstr/dispatch-in-first-arg {:parts 0}) => VString
   (mstr/dispatch-in-first-arg {:parts 0} 3) => VString
   (isa? (mstr/dispatch-in-first-arg "A") MString) => false
   (isa? (mstr/dispatch-in-first-arg {:super 0 :start 0 :end 0}) TString) => true
   (isa? (mstr/dispatch-in-first-arg {:parts 0}) VString) => true))

(testing "`dispatch-for-str`"
  (protocol
   (mstr/dispatch-for-str "") =pred=> #(isa? % JString)
   (mstr/dispatch-for-str "A") =pred=> #(isa? % JString)
   (mstr/dispatch-for-str {:super 0 :start 0 :end 0}) =pred=> #(isa? % MString)
   (mstr/dispatch-for-str 2) => nil
   (mstr/dispatch-for-str 1 2 3) => :multiple-args))

(testing "`str` with regular Clojure data"
  (protocol
   (mstr/str false) =fn=> ->JString "false"
   (mstr/str 23) =fn=> ->JString "23"
   (mstr/str {:a "A"}) =fn=> ->JString "{:a \"A\"}"
   (mstr/str 'a) =fn=> ->JString "a"
   (mstr/str :a) =fn=> ->JString ":a"
   (mstr/str ['a]) =fn=> ->JString "[a]"))

(testing "`str` with Java strings"
  (protocol (mstr/str "") => ""
            (mstr/str "A") => "A"))

(testing "`str` with MStrings"
  (protocol
   (mstr/str (mstr/add-context "A")) => (mstr/add-context "A")))

(testing "`str` with no args"
  (protocol
   (mstr/str) => ""))

(testing "`str` with multiple arguments"
  (protocol
   (mstr/str "a" 1 (mstr/add-context "b") (mstr/str (mstr/add-context "c") "d"))
   =fn=> ->JString "a1bcd"))

(testing "`add-context with Java strings"
  (protocol
   (add-context "X" {:line-number 12}) =fn=> ->JString "X"
   (add-context "X" {:line-number 12}) =fn=> :line-number 12))

(testing "The method `add-context` can be abused to generate a TString."
  (protocol
   (add-context "X") =fn=> ->JString "X"
   (add-context "X") =fn=> mstr/dispatch-in-first-arg TString))

(testing "`add-context` for TStrings"
  (let [s (add-context (add-context "X") {:line-number 12})]
    (protocol
     (->JString s) => "X"
     (s :line-number) => 12)))

(testing "`add-context` for VStrings"
  (let [s (add-context (mstr/str "X" "Y") {:line-number 12})]
    (protocol
     (->JString s) => "XY"
     (s :line-number) => 12)))

;;; Converting back to normal Java strings

(testing "`->JString` with Java strings"
  (protocol
   (->JString "") => ""
   (->JString "A") => "A"))

(testing "`->JString` with Tstrings"
  (protocol
   (->JString (add-context "A")) => "A"
   (->JString (add-context "A" {:line 3})) => "A"))

(testing "`->JString` with VStrings (together with `str`))"
  (protocol
   (-> (mstr/str (add-context "A") (add-context "B") (add-context "C"))
       ->JString) => "ABC"
   (-> (mstr/str (add-context "A") "B")
       ->JString) => "AB"
   (-> (mstr/str "A" (add-context "B"))
       ->JString) => "AB"
   (-> (mstr/str (add-context "A") "")
       ->JString) => "A"
   (-> (mstr/str "A" (add-context ""))
       ->JString) => "A"
   (-> (mstr/str (add-context "") "A")
       ->JString) => "A"
   (-> (mstr/str "" (add-context "A"))
       ->JString) => "A"))

(testing "`index-of` with Java strings"
  (protocol
   (mstr/index-of "ABCBD" "B") => 1
   (mstr/index-of "ABCBD" "B" 2) => 3))

(testing "`index-of` with MStrings"
  (protocol
   (mstr/index-of (mstr/str "AB" "CBD") "B") => 1
   (mstr/index-of (mstr/str "AB" "CBD") "B" 2) => 3))

(testing "`first-index-of`"
  (testing "the general operation"
    (protocol
     (mstr/first-index-of "abdbcefg" ["bc" "b"]) => ["b" 1]
     (mstr/first-index-of "abcdefg" ["b" "bc"]) => ["b" 1]
     (mstr/first-index-of "abcdefg" ["c" "b"]) => ["b" 1]
     (mstr/first-index-of "abcdefg" ["c" "x"]) => ["c" 2]
     (mstr/first-index-of "abcdefg" ["y" "x"]) => nil))
  (testing "with systematic test cases"
    (protocol
     (mstr/first-index-of "abcdefg" ["a"]) => ["a" 0]
     (mstr/first-index-of "abcdefg" ["b"]) => ["b" 1]
     (mstr/first-index-of "abcdefg" ["g"]) => ["g" 6]
     (mstr/first-index-of "abcdefg" ["ab"]) => ["ab" 0]
     (mstr/first-index-of "abcdefg" ["bc"]) => ["bc" 1]
     (mstr/first-index-of "abcdefg" ["fg"]) => ["fg" 5]
     (mstr/first-index-of "abcdefg" ["ab" "a"]) => ["ab" 0]
     (mstr/first-index-of "abcdefg" ["bc" "a"]) => ["a" 0]
     (mstr/first-index-of "abcdefg" ["fg" "a"]) => ["a" 0]
     (mstr/first-index-of "abcdefg" ["a" "b"]) => ["a" 0]
     (mstr/first-index-of "abcdefg" ["b" "c"]) => ["b" 1]
     (mstr/first-index-of "abcdefg" ["g" "f"]) => ["f" 5]
     (mstr/first-index-of "abcdefg" ["ab" "c"]) => ["ab" 0]
     (mstr/first-index-of "abcdefg" ["bc" "d"]) => ["bc" 1]
     (mstr/first-index-of "abcdefg" ["fg" "e"]) => ["e" 4]
     (mstr/first-index-of "abcdefg" ["x" "a"]) => ["a" 0]
     (mstr/first-index-of "abcdefg" ["x" "b"]) => ["b" 1]
     (mstr/first-index-of "abcdefg" ["x" "g"]) => ["g" 6]
     (mstr/first-index-of "abcdefg" ["x" "ab"]) => ["ab" 0]
     (mstr/first-index-of "abcdefg" ["x" "bc"]) => ["bc" 1]
     (mstr/first-index-of "abcdefg" ["x" "fg"]) => ["fg" 5]))
  (testing "with test cases for empty strings"
    (protocol
     (mstr/first-index-of "" []) => nil
     (mstr/first-index-of "" [""]) => ["" 0] ;ok
     (mstr/first-index-of "" ["x"]) => nil
     (mstr/first-index-of "" ["x", "y"]) => nil))
  (testing "with test cases that have no values"
    (protocol
     (mstr/first-index-of "x" []) => nil
     (mstr/first-index-of "x" [""]) => ["" 0])))

(testing "`count` with regular Clojre data"
  (protocol
   (mstr/count [1 2 3]) => 3
   (mstr/count #{1 2 3}) => 3))

(testing "`count` with Java strings"
  (protocol
   (mstr/count "") => 0
   (mstr/count "ABC") => 3))

(testing "`count` with TStrings"
  (protocol
   (mstr/count (add-context "")) => 0
   (mstr/count (add-context "ABC")) => 3))

(testing "`count` with VStrings"
  (protocol
   (mstr/count (mstr/str "AB" "C")) => 3
   (mstr/count (mstr/str (add-context "ABC") "D")) => 4))

(testing "`subs` with Java strings"
  (protocol
   (mstr/subs "ABCDEFG" 2) =fn=> ->JString "CDEFG"
   (mstr/subs "ABCDEFG" 2) =fn=> ->JString "CDEFG"
   (mstr/subs "ABCDEFG" 2 4) =fn=> ->JString "CD"
   (->JString (mstr/subs "ABCDEFG" 10)) =throws=> java.lang.StringIndexOutOfBoundsException))

(testing "`subs` with TStrings"
  (protocol
   (mstr/subs (add-context "ABCDEFG") 2) =fn=> ->JString "CDEFG"
   (mstr/subs (add-context "ABCDEFG") 2 4) =fn=> ->JString "CD"
   (mstr/subs (add-context "ABCDEFG" {:line 17}) 2) =fn=> :line 17
   (mstr/subs (add-context "ABCDEFG" {:line 17}) 2) =fn=> ->JString "CDEFG"
   (->JString (mstr/subs (add-context "ABCDEFG") 10)) =throws=> java.lang.StringIndexOutOfBoundsException))

(testing "`subs` with VStrings"
  (protocol
   (mstr/subs (mstr/str) 0 0) =fn=> ->JString ""
   (mstr/subs (mstr/str "AB") 0 0) =fn=> ->JString ""
   (mstr/subs (mstr/str "AB" "CD") 0 0) =fn=> ->JString ""
   (mstr/subs (mstr/str "EF") 1 1) =fn=> ->JString ""
   (mstr/subs (mstr/str "EF") 2 2) =fn=> ->JString ""
   (mstr/subs (mstr/str "AB" "CD" "EF") 1 5) =fn=> ->JString "BCDE"
   (mstr/subs (mstr/str "AB" "CD" "EF") 0 6) =fn=> ->JString "ABCDEF"
   (mstr/subs (mstr/str "AB" "CD" "EF") 3) =fn=> ->JString "DEF"))

(testing "`split-at` with Clojure collections"
  (protocol
   (mstr/split-at 4 [1 2 3 4 5 6]) => [[1 2 3 4] [5 6]]))

(def all-as-jstr #(map ->JString %))

(testing "`split-at` with Java strings"
  (protocol
   (mstr/split-at 0 "") =fn=> all-as-jstr ["" ""]
   (mstr/split-at 0 "A") =fn=> all-as-jstr ["" "A"]
   (mstr/split-at 1 "A") =fn=> all-as-jstr ["A" ""]
   (mstr/split-at 0 "ABCDEF") =fn=> all-as-jstr ["" "ABCDEF"]
   (mstr/split-at 2 "ABCDEF") =fn=> all-as-jstr ["AB" "CDEF"]
   (mstr/split-at 6 "ABCDEF") =fn=> all-as-jstr ["ABCDEF" ""]
   (mstr/split-at 2 "ABCDEF") ->> (map :super) => ["ABCDEF" "ABCDEF"]))

(testing "`split-at` with TStrings"
  (protocol
   (mstr/split-at 0 (add-context "")) =fn=> all-as-jstr ["" ""]
   (mstr/split-at 0 (add-context "A")) =fn=> all-as-jstr ["" "A"]
   (mstr/split-at 1 (add-context "A")) =fn=> all-as-jstr ["A" ""]
   (mstr/split-at 0 (add-context "ABCDEF")) =fn=> all-as-jstr ["" "ABCDEF"]
   (mstr/split-at 2 (add-context "ABCDEF")) =fn=> all-as-jstr ["AB" "CDEF"]
   (mstr/split-at 6 (add-context "ABCDEF")) =fn=> all-as-jstr ["ABCDEF" ""]
   (mstr/split-at 2 (add-context "ABCDEF")) ->> (map :super) => ["ABCDEF" "ABCDEF"]))

(testing "`split-strings-at` with systematic test cases"
  (let [take-result (fn [result]
                      [(->JString (apply mstr/str (first result)))
                       (->JString (apply mstr/str (second result)))])]
    (protocol
     (mstr/split-strings-at 0 []) =fn=> take-result ["" ""]
     (mstr/split-strings-at 0 ["ABC"]) =fn=> take-result ["" "ABC"]
     (mstr/split-strings-at 0 ["ABC"]) =fn=> take-result ["" "ABC"]
     (mstr/split-strings-at 1 ["ABC"]) =fn=> take-result ["A" "BC"]
     (mstr/split-strings-at 3 ["ABC"]) =fn=> take-result ["ABC" ""]
     (mstr/split-strings-at 0 ["ABC" "DEF"]) =fn=> take-result ["" "ABCDEF"]
     (mstr/split-strings-at 2 ["ABC" "DEF"]) =fn=> take-result ["AB" "CDEF"]
     (mstr/split-strings-at 3 ["ABC" "DEF"]) =fn=> take-result ["ABC" "DEF"]
     (mstr/split-strings-at 4 ["ABC" "DEF"]) =fn=> take-result ["ABCD" "EF"]
     (mstr/split-strings-at 6 ["ABC" "DEFGH"]) =fn=> take-result ["ABCDEF" "GH"]
     (mstr/split-strings-at 2 ["ABC" "DEF" "GHI"]) =fn=> take-result ["AB" "CDEFGHI"]
     (mstr/split-strings-at 3 ["ABC" "DEF" "GHI"]) =fn=> take-result ["ABC" "DEFGHI"]
     (mstr/split-strings-at 5 ["ABC" "DEF" "GHI"]) =fn=> take-result ["ABCDE" "FGHI"]
     (mstr/split-strings-at 9 ["ABC" "DEF" "GHI"]) =fn=> take-result ["ABCDEFGHI" ""])))

(testing "`split-at` with VStrings"
  (protocol
   (mstr/split-at 3 (mstr/str "AB" "CD" "EF")) =fn=> all-as-jstr ["ABC" "DEF"]
   (mstr/split-at 0 (mstr/str "AB" "CD" "EF")) =fn=> all-as-jstr ["" "ABCDEF"]
   (mstr/split-at 6 (mstr/str "AB" "CD" "EF")) =fn=> all-as-jstr ["ABCDEF" ""]))

(testing "`split-at-any-of`"
  (testing "the general operation"
    (protocol
     (mstr/split-at-any-of "a" ["x"]) =fn=> all-as-jstr ["a"]
     (mstr/split-at-any-of "a" ["x" "y"]) =fn=> all-as-jstr ["a"]
     (mstr/split-at-any-of "abcdefgh" ["x"]) =fn=> all-as-jstr ["abcdefgh"]
     (mstr/split-at-any-of "abcdefgh" ["x" "y"]) =fn=> all-as-jstr ["abcdefgh"]
     (mstr/split-at-any-of "abcdefgh" ["a"]) =fn=> all-as-jstr ["" "bcdefgh"]
     (mstr/split-at-any-of "abcdefgh" ["h"]) =fn=> all-as-jstr ["abcdefg" ""]
     (mstr/split-at-any-of "abcdefgh" ["c" "f"]) =fn=> all-as-jstr ["ab" "de" "gh"]
     (mstr/split-at-any-of "abcdefgh" ["c" "f"]) =fn=> all-as-jstr ["ab" "de" "gh"]
     (mstr/split-at-any-of "abcdefgh" ["c" "f"] :line-no) ->> (map :line-no) => [0 1 2]
     (mstr/split-at-any-of "abcdefgh" ["c" "f"] :line-no 12) ->> (map :line-no) => [12 13 14]))
  (testing "with test cases about for empty strings"
    (protocol
     (mstr/split-at-any-of "" []) =fn=> all-as-jstr [""]
     (mstr/split-at-any-of "" [""]) =throws=> java.lang.AssertionError
     (mstr/split-at-any-of "" ["x"]) =fn=> all-as-jstr [""]
     (mstr/split-at-any-of "X" []) =fn=> all-as-jstr ["X"]
     (mstr/split-at-any-of "X" [""]) =throws=> java.lang.AssertionError))
  (testing "with other MString types"
    (protocol
     (mstr/split-at-any-of (add-context "abcdefgh") ["c" "f"]) =fn=> all-as-jstr ["ab" "de" "gh"]
     (mstr/split-at-any-of (mstr/str "abcd" "efgh") ["c" "f"]) =fn=> all-as-jstr ["ab" "de" "gh"])))

(testing "`split-pages`"
  (testing "with a real world example"
    (protocol
     (mstr/split-pages "page1\f\r\npage2\f\n\rpage3\f\npage4\f\rpage5\fpage6")
     ->> (map (juxt :page-no ->JString))
     => [[0 "page1"] [1 "page2"] [2 "\rpage3"] [3 "page4"] [4 "page5"] [5 "page6"]]))
  (testing "with simple test cases"
    (protocol
     (mstr/split-pages "x") =fn=> all-as-jstr ["x"]
     (mstr/split-pages "x\fy") =fn=> all-as-jstr ["x" "y"]
     (mstr/split-pages "x\fy\fz") =fn=> all-as-jstr ["x" "y" "z"]))
  (testing "with test cases for all sorts of page breaks"
    (protocol
     (mstr/split-pages "x\fy") =fn=> all-as-jstr ["x" "y"]
     (mstr/split-pages "x\f\ry") =fn=> all-as-jstr ["x" "y"]
     (mstr/split-pages "x\f\ny") =fn=> all-as-jstr ["x" "y"]
     (mstr/split-pages "x\f\r\ny") =fn=> all-as-jstr ["x" "y"]
     (mstr/split-pages "x\f\n\ry") =fn=> all-as-jstr ["x" "\ry"]))
  (testing "with test cases for all sorts of page breaks"
    (protocol
     (mstr/split-pages "x\fy") =fn=> all-as-jstr ["x" "y"]
     (mstr/split-pages "x\f\ry") =fn=> all-as-jstr ["x" "y"]
     (mstr/split-pages "x\f\ny") =fn=> all-as-jstr ["x" "y"]
     (mstr/split-pages "x\f\r\ny") =fn=> all-as-jstr ["x" "y"]
     (mstr/split-pages "x\f\n\ry") =fn=> all-as-jstr ["x" "\ry"]))
  (testing "with test cases that confuse with empty pages"
    (protocol
     (mstr/split-pages "") =fn=> all-as-jstr [""]
     (mstr/split-pages "\f") =fn=> all-as-jstr ["" ""]
     (mstr/split-pages "\f\r\n") =fn=> all-as-jstr ["" ""]
     (mstr/split-pages "\f\r\nx") =fn=> all-as-jstr ["" "x"]
     (mstr/split-pages "\f\n") =fn=> all-as-jstr ["" ""]
     (mstr/split-pages "\f\f\f") =fn=> all-as-jstr ["" "" "" ""]
     (mstr/split-pages "x\f\f\f") =fn=> all-as-jstr ["x" "" "" ""]
     (mstr/split-pages "\fx\f\f") =fn=> all-as-jstr ["" "x" "" ""]
     (mstr/split-pages "\f\f\fx") =fn=> all-as-jstr ["" "" "" "x"]
     (mstr/split-pages "\f\n\f\n\f\n") =fn=> all-as-jstr ["" "" "" ""]
     (mstr/split-pages "x\f\n\f\n\f\n") =fn=> all-as-jstr ["x" "" "" ""]
     (mstr/split-pages "\f\nx\f\n\f\n") =fn=> all-as-jstr ["" "x" "" ""]
     (mstr/split-pages "\f\n\f\n\f\nx") =fn=> all-as-jstr ["" "" "" "x"]
     (mstr/split-pages "\f\n\f\n\f\r\n\n") =fn=> all-as-jstr ["" "" "" "\n"])))


(testing "`split-lines`"
  (testing "with a real world example"
    (protocol
     (mstr/split-lines "line1\nline2\r\nline3\rline4")
     ->> (map (juxt :line-no ->JString))
     => [[0 "line1"] [1 "line2"] [2 "line3"] [3 "line4"]]))
  (testing "with different line numbers"
    (protocol
     (mstr/split-lines "line1\nline2\r\nline3\rline4" 7)
     ->> (map (juxt :line-no ->JString))
     => [[7 "line1"] [8 "line2"] [9 "line3"] [10 "line4"]]))
  (testing "with simple test cases"
    (protocol
     (mstr/split-lines "x") =fn=> all-as-jstr ["x"]
     (mstr/split-lines "x\ny") =fn=> all-as-jstr ["x" "y"]
     (mstr/split-lines "x\ny\nz") =fn=> all-as-jstr ["x" "y" "z"]))
  (testing "with test cases for all sorts of line breaks"
    (protocol
     (mstr/split-lines "x\ny") =fn=> all-as-jstr ["x" "y"]
     (mstr/split-lines "x\n\ry") =fn=> all-as-jstr ["x" "" "y"]
     (mstr/split-lines "x\r\ny") =fn=> all-as-jstr ["x" "y"]))
  (testing "with test cases that confuse with empty lines"
    (protocol
     (mstr/split-lines "") =fn=> all-as-jstr [""]
     (mstr/split-lines "\n") =fn=> all-as-jstr ["" ""]
     (mstr/split-lines "\r") =fn=> all-as-jstr ["" ""]
     (mstr/split-lines "\r\n") =fn=> all-as-jstr ["" ""]
     (mstr/split-lines "\n\r") =fn=> all-as-jstr ["" "" ""]
     (mstr/split-lines "\r\n\r") =fn=> all-as-jstr ["" "" ""]
     (mstr/split-lines "\n\r\r") =fn=> all-as-jstr ["" "" "" ""]
     (mstr/split-lines "\n\n\r") =fn=> all-as-jstr ["" "" "" ""]
     (mstr/split-lines "\r\r\n") =fn=> all-as-jstr ["" "" ""]
     (mstr/split-lines "\r\n\r") =fn=> all-as-jstr ["" "" ""])))

(testing "testing `split-pages-and-lines`"
  (testing "with a real world example"
    (let [example (mstr/split-pages-and-lines "a\nb\f\nc")]
      (protocol
       example ->> (map :line-no) => [0 1 2]
       example ->> (map :page-no) => [0 0 1]
       example ->> (map ->JString) => ["a" "b" "c"])))
  (testing "with some exotic situations"
    (let [example (mstr/split-pages-and-lines "a\f\n\rb")]
      (protocol
       example ->> (map :line-no) => [0 1 2]
       example ->> (map :page-no) => [0 1 1]
       example ->> (map ->JString) => ["a" "" "b"]))
    (let [example (mstr/split-pages-and-lines "\f\n\na\nb\f\nc")]
      (protocol
       example ->> (map :line-no) => [0 1 2 3 4]
       example ->> (map :page-no) => [0 1 1 1 2]
       example ->> (map ->JString) => ["" "" "a" "b" "c"]))
    (let [example (mstr/split-pages-and-lines "\na\nb\f\nc")]
      (protocol
       example ->> (map :line-no) => [0 1 2 3]
       example ->> (map :page-no) => [0 0 0 1]
       example ->> (map ->JString) => ["" "a" "b" "c"]))
    (let [example (mstr/split-pages-and-lines "a\n\nb\f\nc")]
      (protocol
       example ->> (map :line-no) => [0 1 2 3]
       example ->> (map :page-no) => [0 0 0 1]
       example ->> (map ->JString) => ["a" "" "b" "c"]))
    (let [example (mstr/split-pages-and-lines "a\nb\f\nc\n")]
      (protocol
       example ->> (map :line-no) => [0 1 2 3]
       example ->> (map :page-no) => [0 0 1 1]
       example ->> (map ->JString) => ["a" "b" "c" ""]))
    (let [example (mstr/split-pages-and-lines "a\nb\f\nc\f\n\n")]
      (protocol
       example ->> (map :line-no) => [0 1 2 3 4]
       example ->> (map :page-no) => [0 0 1 2 2]
       example ->> (map ->JString) => ["a" "b" "c" "" ""]))))


(testing "acceptance tests for `split-lines`"
  (testing "with artificial example"
    (protocol
     (mstr/split-pages-and-lines (slurp "test/TestData/LineAndPageBreaks.txt"))
     ->> (map ->JString) (map #(clojure.core/str (inc %1) ": " %2) (range))
     => (clojure.string/split-lines (slurp "test/TestData/LineAndPageBreaks_expected.txt"))))
  (testing "with real example"
    (let [pages (->> "test/TestData/JCOBITCP_JCOBTCC.LIS"
                     slurp
                     mstr/split-pages-and-lines
                     (group-by :page-no)
                     vals
                     (mapv all-as-jstr))]
      (protocol
       (nth pages 0) => (clojure.string/split (slurp "test/TestData/JCOBITCP_JCOBTCC_expected_page_1.LIS") #"\r?\n" -1)
       (nth pages 1) => (clojure.string/split (slurp "test/TestData/JCOBITCP_JCOBTCC_expected_page_2.LIS") #"\r?\n" -1)
       (nth pages 2) => (clojure.string/split (slurp "test/TestData/JCOBITCP_JCOBTCC_expected_page_3.LIS") #"\r?\n" -1)
       (nth pages 3) => (clojure.string/split (slurp "test/TestData/JCOBITCP_JCOBTCC_expected_page_4.LIS") #"\r?\n" -1)
       (nth pages 4) => (clojure.string/split (slurp "test/TestData/JCOBITCP_JCOBTCC_expected_page_5.LIS") #"\r?\n" -1)
       (nth pages 5) => (clojure.string/split (slurp "test/TestData/JCOBITCP_JCOBTCC_expected_page_6.LIS") #"\r?\n" -1)
       (nth pages 6) => (clojure.string/split (slurp "test/TestData/JCOBITCP_JCOBTCC_expected_page_7.LIS") #"\r?\n" -1)))))

(testing "`empty?`"
  (protocol
   (mstr/empty? nil) => true
   (mstr/empty? []) => true
   (mstr/empty? [1]) => false
   (mstr/empty? "") => true
   (mstr/empty? " ") => false
   (mstr/empty? "x") => false
   (mstr/empty? (add-context "")) => true
   (mstr/empty? (add-context " ")) => false
   (mstr/empty? (add-context "X")) => false
   (mstr/empty? (mstr/str "" "")) => true
   (mstr/empty? (mstr/str "X" "")) => false
   (mstr/empty? (mstr/str "X" "Y")) => false
   (mstr/empty? (mstr/str "" "Y")) => false
   (mstr/empty? (mstr/str (add-context "") (add-context ""))) => true
   (mstr/empty? (mstr/str (add-context "X") (add-context ""))) => false
   (mstr/empty? (mstr/str (add-context "X") (add-context "Y"))) => false
   (mstr/empty? (mstr/str (add-context "") (add-context "Y"))) => false))

(testing "`empty?`"
  (protocol
   (mstr/first nil) => nil
   (mstr/first []) => nil
   (mstr/first [1]) => 1
   (mstr/first "") => nil
   (mstr/first " ") => \space
   (mstr/first "X") => \X
   (mstr/first (add-context "")) => nil
   (mstr/first (add-context " ")) => \space
   (mstr/first (add-context "X")) => \X
   (mstr/first (add-context "XY")) => \X
   (mstr/first (mstr/str "" "")) => nil
   (mstr/first (mstr/str "X" "")) => \X
   (mstr/first (mstr/str "X" "Y")) => \X
   (mstr/first (mstr/str "" "Y")) => \Y
   (mstr/first (mstr/str (add-context "") (add-context ""))) => nil
   (mstr/first (mstr/str (add-context "X") (add-context ""))) => \X
   (mstr/first (mstr/str (add-context "X") (add-context "Y"))) => \X
   (mstr/first (mstr/str (add-context "") (add-context "Y"))) => \Y))

(testing "`last`"
  (protocol
   (mstr/last nil) => nil
   (mstr/last []) => nil
   (mstr/last [1]) => 1
   (mstr/last "") => nil
   (mstr/last " ") => \space
   (mstr/last "X") => \X
   (mstr/last (add-context "")) => nil 
   (mstr/last (add-context " ")) => \space
   (mstr/last (add-context "X")) => \X
   (mstr/last (add-context "XY")) => \Y
   (mstr/last (mstr/str "" "")) => nil
   (mstr/last (mstr/str "X" "")) => \X
   (mstr/last (mstr/str "X" "Y")) => \Y
   (mstr/last (mstr/str "" "Y")) => \Y
   (mstr/last (mstr/str (add-context "") (add-context ""))) => nil
   (mstr/last (mstr/str (add-context "X") (add-context ""))) => \X
   (mstr/last (mstr/str (add-context "X") (add-context "Y"))) => \Y
   (mstr/last (mstr/str (add-context "") (add-context "Y"))) => \Y))

(testing "`starts-with?"
  (protocol
   (mstr/starts-with? "ABCD" "A") => true
   (mstr/starts-with? "ABCD" "B") => false
   (mstr/starts-with? "ABCD" "") => true
   (mstr/starts-with? (add-context "ABCD") "A") => true
   (mstr/starts-with? (add-context "ABCD") "B") => false
   (mstr/starts-with? (add-context "ABCD") "") => true
   (mstr/starts-with? (mstr/str "AB" "CD") "A") => true
   (mstr/starts-with? (mstr/str "AB" "CD") "B") => false
   (mstr/starts-with? (mstr/str "AB" "CD") "") => true))