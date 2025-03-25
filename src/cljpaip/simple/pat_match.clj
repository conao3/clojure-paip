(ns cljpaip.simple.pat-match
  (:require
   [cljpaip.simple.util :refer [cdr variable?]]))

(defn match-variable [sym input bindings]
  (cond
    (find bindings sym) (when (= (sym bindings) input) bindings)
    :else (assoc bindings sym input)))

(defn pat-match
  ([pattern input]
   (pat-match pattern input {}))
  ([pattern input bindings]
   (cond
     (nil? bindings) nil
     (= pattern input) bindings
     (variable? pattern) (match-variable pattern input bindings)
     (and (sequential? pattern) (sequential? input))
     (recur (cdr pattern) (cdr input)
            (pat-match (first pattern) (first input) bindings)))))
