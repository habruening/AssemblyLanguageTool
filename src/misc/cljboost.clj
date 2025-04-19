(ns misc.cljboost)

(defn as-max-function [comp]
  (fn maxfunc ([x] x)
    ([x y] (if (comp x y) y x))
    ([x y & more]
     (reduce maxfunc (maxfunc x y) more))))

(defn as-min-function [comp]
  (fn minfunc ([x] x)
    ([x y] (if (comp x y) x y))
    ([x y & more]
     (reduce minfunc (minfunc x y) more))))

(defn implies [a b]
  (or (not a) b))

(defn one-by-one! [coll]
  (let [state (atom coll)]
    (fn []
      (when-let [current (first @state)]
        (swap! state rest)
        current))))

(defn parse-int [s]
  (when (string? s)
    (if (clojure.string/starts-with? s "0x")
      (Integer/parseInt (clojure.core/subs s 2) 16)
      (Integer/parseInt s))))

(parse-int "0xA3")