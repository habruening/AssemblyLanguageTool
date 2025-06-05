(ns asslangtool.assembly-listing
  (:require [misc.mstring :as mstr]
            [misc.cljboost :refer :all]))

(defn parse-label [line]
  (if-let [label (re-find #"^                                   ([a-zA-Z0-9_$]+): *$" (mstr/->JString line))] 
    (mstr/subs line 35 (+ 35 (count (second label))))))

(defn parse-instruction [line]
  (if (and (mstr/starts-with? line "     ")
           (<= 40 (mstr/count line))
           (= (mstr/->JString (mstr/subs line 9 11)) "  ")
           (re-find #"^[0-9A-F]+$" (mstr/->JString (mstr/subs line 5 9))))
    {:checkspace (mstr/subs line 0 5)
     :address (mstr/subs line 5 9)
     :opcode (mstr/subs line 11 39)
     :mnemonic (mstr/subs line 39 51)
     :arguments (mstr/subs line 51)}))

(defn assembly-listing [lines]
  (first (reduce
          (fn [[{labels :labels code :code :as result} labels-before-line :as cont] line]
            (if-let [parsed (or (parse-label line) (parse-instruction line))]
              (if (:opcode parsed)
                (let [address (-> parsed :address mstr/->JString (Integer/parseInt 16))]
                  [{:labels (reduce #(assoc %1 (mstr/->JString %2) address) labels labels-before-line)
                    :code (assoc code address parsed)}
                   #{}])
                [result (conj labels-before-line parsed)])
              cont))
          [{:labels {} :code {}} #{}]
          lines)))

(defn listing-file-for-testing-purposes [start-address end-address]
  (let [no-of-address-characters (count (format "%x" end-address))
        address-format (str "%0" no-of-address-characters "x")
        listing-file (->> (range start-address end-address)
                          (map #(clojure.string/upper-case (format address-format %)))
                          (map #(str "      " %))
                          (clojure.string/join "\n"))
        instruction-list (->> listing-file
                              (mstr/split-lines)
                              (reduce #(assoc %1
                                              (Integer/parseInt (mstr/->JString (mstr/subs %2 6 (+ 6 no-of-address-characters))) 16)
                                              {:checkspace (mstr/subs %2 0 5)
                                               :mnemonic ""})
                                      {}))]
    [listing-file instruction-list]))

(comment
  (listing-file-for-testing-purposes 3000 3005)
  )



