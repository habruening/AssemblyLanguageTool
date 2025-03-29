(ns misc.testboost)

(defn test-and-expect [[expr arrow & args]]
  (cond (= arrow '=>) [`(clojure.test/is (= ~expr ~(first args)))
                       (rest args)]
        (= arrow '=fn=>) [`(clojure.test/is (= (~(first args) ~expr) ~(second args)))
                          (nthrest args 2)]
        (= arrow '=pred=>) [`(clojure.test/is (~(first args) ~expr))
                            (rest args)]
        (= arrow '=throws=>) [`(clojure.test/is (~'thrown? ~(first args) ~expr))
                              (rest args)]
        (contains? #{'-> '->>} arrow) (let [[func continuation] (split-with #(not (contains? #{'=> '=fn=> '=pred=> '=throws=> '-> '->>} %)) args)]
                                       (test-and-expect (into [(apply list arrow expr func)] continuation)))))

(defmacro protocol [& args]
  (loop [lines []
         more-args args]
    (if (empty? more-args) lines
        (let [[test still-more-args] (test-and-expect more-args)]
          (recur (conj lines test)
                 still-more-args)))))
