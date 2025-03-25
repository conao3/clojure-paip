(ns cljpaip.simple.unify
  (:require
   [cljpaip.simple.util :refer [cdr variable?]]))

(defn unify-variable [sym x bindings]
  (cond
    (find bindings sym) (when (= (sym bindings) x) bindings)
    ;; (find bindings sym) (unify (sym bindings) x bindings)
    (and (variable? x) (find bindings x)) (when (= sym (x bindings)) bindings)
    :else (assoc bindings sym x)))

(defn unify
  ([x y]
   (unify x y {}))
  ([x y bindings]
   (cond
     (nil? bindings) nil
     (= x y) bindings
     (variable? x) (unify-variable x y bindings)
     (variable? y) (unify-variable y x bindings)
     (and (sequential? x) (sequential? y))
     (recur (cdr x) (cdr y)
            (unify (first x) (first y) bindings)))))
