(ns cljpaip.simple.util)

(defmacro trap
  ([body]
   `(trap ~body nil))
  ([body fallback]
   `(try
      ~body
      (catch Exception _#
        ~fallback))))

(defn cdr
  "commonlisp like cdr
  care about notation of cons-cell.

  nil => nil
  (a) => nil
  (a . nil) => nil
  (a . ?x) => ?x"
  [v]
  (let [r (next v)]
    (cond-> r
      (and (seq? r) (= '. (first r)))
      ((fn [a]
         (when (not= 2 (count a))
           (throw (ex-info "exact 1 element after `.'" {})))
         (second a))))))

(defn variable? [x]
  (-> (some->> (trap (name x)) (re-find #"^\?"))
      some?))
