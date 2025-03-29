(ns misc.mstring-test
  (:refer-clojure :exclude [str subs count])
  (:require [midje.sweet :refer :all]
            [clojure.test :refer :all]
            [misc.testboost :refer :all]
            [misc.mstring :as mstr :refer [JString TString VString MString str ->JString add-context subs split-at]]))


;;; The standard dispatcher for all string types

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

;;; the operator clojure.core/str reimplemented

(testing "`dispatch-for-str`"
  (protocol
   (mstr/dispatch-for-str "") =pred=> #(isa? % JString)
   (mstr/dispatch-for-str "A") =pred=> #(isa? % JString)
   (mstr/dispatch-for-str {:super 0 :start 0 :end 0}) =pred=> #(isa? % MString)
   (mstr/dispatch-for-str 2) => nil
   (mstr/dispatch-for-str 1 2 3) => :multiple-args))

(testing "`str` with regular Clojure data"
  (protocol
   (str false) =fn=> ->JString "false"
   (str 23) =fn=> ->JString "23"
   (str {:a "A"}) =fn=> ->JString "{:a \"A\"}"
   (str 'a) =fn=> ->JString "a"
   (str :a) =fn=> ->JString ":a"
   (str ['a]) =fn=> ->JString "[a]"))

(testing "`str` with Java strings"
  (protocol (str "") => ""
            (str "A") => "A"))

(testing "`str` with MStrings"
  (protocol
   (str (mstr/add-context "A")) => (mstr/add-context "A")))

(testing "`str` with no args"
  (protocol
   (str) => ""))

(testing "`str` with multiple arguments"
  (protocol
   (str "a" 1 (mstr/add-context "b") (str (mstr/add-context "c") "d"))
   =fn=> ->JString "a1bcd"))

;;; Strings with context (e.g. line numbers).

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
  (let [s (add-context (str "X" "Y") {:line-number 12})]
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
   (-> (str (add-context "A") (add-context "B") (add-context "C"))
       ->JString) => "ABC"
   (-> (str (add-context "A") "B")
       ->JString) => "AB"
   (-> (str "A" (add-context "B"))
       ->JString) => "AB"
   (-> (str (add-context "A") "")
       ->JString) => "A"
   (-> (str "A" (add-context ""))
       ->JString) => "A"
   (-> (str (add-context "") "A")
       ->JString) => "A"
   (-> (str "" (add-context "A"))
       ->JString) => "A"))

;;; the function clojure.string/index-of reimplemented

(testing "`index-of` with Java strings"
  (protocol
   (mstr/index-of "ABCBD" "B") => 1
   (mstr/index-of "ABCBD" "B" 2) => 3))

(testing "`index-of` with MStrings"
  (protocol
   (mstr/index-of (str "AB" "CBD") "B") => 1
   (mstr/index-of (str "AB" "CBD") "B" 2) => 3))

;;; searching multiple characters

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

;;; the function clojure.core/count reimplemented

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
   (mstr/count (str "AB" "C")) => 3
   (mstr/count (str (add-context "ABC") "D")) => 4))

;;; the function clojure.core/subs reimplemented

(testing "`subs` with Java strings"
  (protocol
   (subs "ABCDEFG" 2) =fn=> ->JString "CDEFG"
   (subs "ABCDEFG" 2) =fn=> ->JString "CDEFG"
   (subs "ABCDEFG" 2 4) =fn=> ->JString "CD"
   (->JString (subs "ABCDEFG" 10)) =throws=> java.lang.StringIndexOutOfBoundsException))

(testing "`subs` with TStrings"
  (protocol
   (subs (add-context "ABCDEFG") 2) =fn=> ->JString "CDEFG"
   (subs (add-context "ABCDEFG") 2 4) =fn=> ->JString "CD"
   (subs (add-context "ABCDEFG" {:line 17}) 2) =fn=> :line 17
   (subs (add-context "ABCDEFG" {:line 17}) 2) =fn=> ->JString "CDEFG"
   (->JString (subs (add-context "ABCDEFG") 10)) =throws=> java.lang.StringIndexOutOfBoundsException))

(testing "`subs` with VStrings"
  (protocol
   (subs (str) 0 0) =fn=> ->JString ""
   (subs (str "AB") 0 0) =fn=> ->JString ""
   (subs (str "AB" "CD") 0 0) =fn=> ->JString ""
   (subs (str "EF") 1 1) =fn=> ->JString ""
   (subs (str "EF") 2 2) =fn=> ->JString ""
   (subs (str "AB" "CD" "EF") 1 5) =fn=> ->JString "BCDE"
   (subs (str "AB" "CD" "EF") 0 6) =fn=> ->JString "ABCDEF"
   (subs (str "AB" "CD" "EF") 3) =fn=> ->JString "DEF"))

;;; the function clojure.core/split-at reimplemented

(testing "`split-at` with Clojure collections"
  (protocol
   (split-at 4 [1 2 3 4 5 6]) => [[1 2 3 4] [5 6]]))

(def all-as-jstr #(map ->JString %))

(testing "`split-at` with Java strings"
  (protocol
   (split-at 0 "") =fn=> all-as-jstr ["" ""]
   (split-at 0 "A") =fn=> all-as-jstr ["" "A"]
   (split-at 1 "A") =fn=> all-as-jstr ["A" ""]
   (split-at 0 "ABCDEF") =fn=> all-as-jstr ["" "ABCDEF"]
   (split-at 2 "ABCDEF") =fn=> all-as-jstr ["AB" "CDEF"]
   (split-at 6 "ABCDEF") =fn=> all-as-jstr ["ABCDEF" ""]
   (split-at 2 "ABCDEF") ->> (map :super) => ["ABCDEF" "ABCDEF"]))

(testing "`split-at` with TStrings"
  (protocol
   (split-at 0 (add-context "")) =fn=> all-as-jstr ["" ""]
   (split-at 0 (add-context "A")) =fn=> all-as-jstr ["" "A"]
   (split-at 1 (add-context "A")) =fn=> all-as-jstr ["A" ""]
   (split-at 0 (add-context "ABCDEF")) =fn=> all-as-jstr ["" "ABCDEF"]
   (split-at 2 (add-context "ABCDEF")) =fn=> all-as-jstr ["AB" "CDEF"]
   (split-at 6 (add-context "ABCDEF")) =fn=> all-as-jstr ["ABCDEF" ""]
   (split-at 2 (add-context "ABCDEF")) ->> (map :super) => ["ABCDEF" "ABCDEF"]))

(testing "`split-strings-at` with systematic test cases"
  (let [take-result (fn [result]
                      [(->JString (apply str (first result)))
                       (->JString (apply str (second result)))])]
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
   (split-at 3 (str "AB" "CD" "EF")) =fn=> all-as-jstr ["ABC" "DEF"]
   (split-at 0 (str "AB" "CD" "EF")) =fn=> all-as-jstr ["" "ABCDEF"]
   (split-at 6 (str "AB" "CD" "EF")) =fn=> all-as-jstr ["ABCDEF" ""]))

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
     (mstr/split-at-any-of (str "abcd" "efgh") ["c" "f"]) =fn=> all-as-jstr ["ab" "de" "gh"])))

(testing "`split-pages`"
  (testing "with a real world example"
    (protocol
     (mstr/split-pages "page1\f\r\npage2\f\n\rpage3\f\npage4\f\rpage5\fpage6")
     ->> (map (juxt :page-no ->JString))
     => [[0 "page1"] [1 "page2"] [2 "page3"] [3 "page4"] [4 "page5"] [5 "page6"]]))
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
     (mstr/split-pages "x\f\n\ry") =fn=> all-as-jstr ["x" "y"]))
  (testing "with test cases for all sorts of page breaks"
    (protocol
     (mstr/split-pages "x\fy") =fn=> all-as-jstr ["x" "y"]
     (mstr/split-pages "x\f\ry") =fn=> all-as-jstr ["x" "y"]
     (mstr/split-pages "x\f\ny") =fn=> all-as-jstr ["x" "y"]
     (mstr/split-pages "x\f\r\ny") =fn=> all-as-jstr ["x" "y"]
     (mstr/split-pages "x\f\n\ry") =fn=> all-as-jstr ["x" "y"]))
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