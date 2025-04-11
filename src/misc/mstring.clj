(ns misc.mstring
  (:refer-clojure :exclude [str subs count split-at empty? first last])
  (:require [misc.cljboost :refer :all]))

(def JString "Normal Java strings" ::JString)
(def TString "Substrings of Java strings" ::TString)
(def VString "Concatenated strings" ::VString)
(def MString "Any string type" ::MString)
(def NoString "Not a string" ::NoString)

(derive TString MString)
(derive VString MString)

(defn dispatch-in-first-arg [s & _]
  (cond (= (type s) java.lang.String)
        JString
        (and (= (type s) clojure.lang.PersistentArrayMap)
             (s :super)
             (s :start)
             (s :end))
        TString
        (and (= (type s) clojure.lang.PersistentArrayMap)
             (s :parts))
        VString))

(defn dispatch-for-str [& args]
  (cond (clojure.core/empty? args)
        :no-args
        (< 1 (clojure.core/count args))
        :multiple-args
        :else
        (dispatch-in-first-arg (clojure.core/first args))))

(defmulti str dispatch-for-str)

(defmethod str nil [s]
  (clojure.core/str s))

(defmethod str JString [s]
  s)

(defmethod str MString [s]
  s)

(defmethod str :no-args [] "")

(defmethod str :multiple-args [& args]
  ; todo: remove empty strings
  {:parts (mapv str args)})

; Note: MStrings cannot be created. They occur. For example by
; creating substrings or by adding a context to a string with
; `add-context`. But `add-contex` can also abused to create
; MStrings (perhaps for Unit Testing).

(defmulti add-context dispatch-in-first-arg)

(defmethod add-context JString
  ([s context] (merge {:super s :start 0 :end (clojure.core/count s)}
                      context))
  ([s] (add-context s {})))

(defmethod add-context MString [s context]
  (merge s context))

;;; Converting back to normal Java strings

;; It would be nice to just use clojure.core/str. But that is (currently)
;; not possible. We have overdefined it. And even if we call it, we would
;; not see the string as a string, but as a cryptic map. We would have to
;; define it as an `Object` and implement `.toString`.

;; We don't intend this and don't provide full compatibility with Clojure
;; strings. The function clojure.core/str will (probably never) work, and
;; the equality will treat MStrings as maps, but not like strings.

;; Trying to changing this could be problematic in unforseeen areas. We
;; don't try this. We don't try to fully mimic Clojure strings. We are
;; clear by saying that MStrings have no nice value semantics. This is,
;; because they are not values. Users of MStrings must know and understand
;; this. That is why we convert them to Clojure strings only with that
;; clumsily named method `->JString` and not in a more comfortable way.

(defmulti ->JString dispatch-in-first-arg)

(defmethod ->JString JString [s] s)

(defmethod ->JString TString [s]
  (.substring (s :super) (s :start) (s :end)))

(defmethod ->JString VString [s]
  (clojure.string/join (map ->JString (s :parts))))

(defn index-of
  ([s value]
   (clojure.string/index-of (->JString s) value))
  ([s value from-index]
   (clojure.string/index-of (->JString s) value from-index)))

(defn first-index-of [s values]
  (let [indices (->> values
                     (mapv (partial index-of s))
                     (mapv vector values)
                     (filter second))]
    (if (seq indices)
      (apply (as-max-function #(< (second %2) (second %1))) indices))))

(defmulti count dispatch-in-first-arg)

(defmethod count nil [coll]
  (clojure.core/count coll))

(defmethod count JString [s]
  (clojure.core/count s))

(defmethod count TString [s]
  (- (s :end) (s :start)))

(defmethod count VString [s]
  (reduce #(+ %1 (count %2)) 0 (s :parts)))

(defmulti subs dispatch-in-first-arg)

(defmethod subs JString
  ([s start]
   (subs s start (count s)))
  ([s start end]
   {:super s :start start :end end}))

(defmethod subs TString
  ([s start]
   (assoc s :start (+ (s :start) start)))
  ([s start end]
   (assoc s :start (+ (s :start) start) :end (+ (s :start) end))))

(def split-vstring-at-index) ; defined below

(defmulti split-at #(dispatch-in-first-arg %2))

(defmethod subs VString
  ([s start]
   (subs s start (count s)))
  ([s start end]
   (->> s
        (split-at start)
        second
        (split-at (- end start))
        clojure.core/first)))

(defmethod split-at nil [n coll]
  (clojure.core/split-at n coll))

(defmethod split-at JString [n s]
  [(subs s 0 n) (subs s n (count s))])

(defmethod split-at TString [n s]
  [(subs s 0 n) (subs s n (count s))])

(defn split-strings-at [n all-s]
  (loop [left-s []
         right-s all-s
         cut n]
    (cond (clojure.core/empty? right-s)
          [left-s []]
          (= cut 0)
          [left-s right-s]
          (< cut (count (clojure.core/first right-s)))
          [(conj left-s (subs (clojure.core/first right-s) 0 cut))
           (assoc right-s 0 (subs (clojure.core/first right-s) cut))]
          :else
          (recur (conj left-s (clojure.core/first right-s))
                 (subvec right-s 1)
                 (- cut (count (clojure.core/first right-s)))))))

(defmethod split-at VString [n s]
  (let [[left-s right-s] (split-strings-at n (s :parts))]
    [(apply str left-s) (apply str right-s)]))

(defn split-at-any-of
  ([s values]
   (split-at-any-of s values nil))
  ([s values numbering-key]
   (split-at-any-of s values numbering-key 0))
  ([s values numbering-key first-number]
   (doseq [value values] (assert (and (string? value)
                                      (not (= value "")))
                                 "Cannot split at \"\"."))
   (loop [parts []
          remaining s
          number first-number]
     (let [[separator sep-index] (first-index-of remaining values)
           set-number (if numbering-key
                        #(add-context % {numbering-key number})
                        identity)]
       (if (not sep-index)
         (conj parts (set-number remaining))
         (recur (conj parts (set-number (subs remaining 0 sep-index)))
                (subs remaining (+ sep-index (count separator)))
                (inc number)))))))

(comment "See how it works in this example."
         (split-at-any-of (str "abcd" "efgh") ["c" "f"]))

(defn split-pages [s]
  (split-at-any-of s ["\f\r\n" #_"\f\n\r" "\f\n" "\f\r" "\f"] :page-no))

(defn split-lines
  ([s] (split-lines s 0))
  ([s first-line-number]
   (split-at-any-of s ["\r\n" "\n" "\r"] :line-no first-line-number)))


(defn split-pages-and-lines [s]
  (loop [lines []
         last-line-no -1
         remaining-pages (split-pages s)]
    (if (clojure.core/empty? remaining-pages)
      lines
      (let [lines-of-page (split-lines (clojure.core/first remaining-pages) (inc last-line-no))]
        (recur (into lines lines-of-page)
               (-> lines-of-page clojure.core/last :line-no)
               (rest remaining-pages))))))

(defmulti empty? dispatch-in-first-arg)

(defmethod empty? nil [coll]
  (clojure.core/empty? coll))

(defmethod empty? JString [coll]
  (clojure.core/empty? coll))

(defmethod empty? MString [coll]
  (= (coll :start) (coll :end)))

(defmethod empty? VString [coll]
  (every? empty? (coll :parts)))

(defmulti first dispatch-in-first-arg)

(defmethod first nil [coll]
  (clojure.core/first coll))

(defmethod first JString [coll]
  (clojure.core/first coll))

(defmethod first MString [coll]
  (if (not (empty? coll))
    (nth (coll :super) (coll :start))))

(defmethod first VString [coll]
  (if-let [first-non-empty (some #(if (not (empty? %)) %) (coll :parts))]
    (first first-non-empty)))

(defmulti last dispatch-in-first-arg)

(defmethod last nil [coll]
  (clojure.core/last coll))

(defmethod last JString [coll]
  (clojure.core/last coll))

(defmethod last MString [coll]
  (if (not (empty? coll))
    (nth (coll :super) (dec (coll :end)))))

(defmethod last VString [coll]
  (if-let [first-non-empty (some #(if (not (empty? %)) %) (reverse (coll :parts)))]
    (last first-non-empty)))

(defn starts-with? [s substr]
  (clojure.string/starts-with? (->JString s) (->JString substr)))
