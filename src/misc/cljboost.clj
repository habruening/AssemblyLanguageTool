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

