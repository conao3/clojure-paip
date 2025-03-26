(ns cljpaip.simple.unify
  (:require
   [cljpaip.simple.util :refer [cdr variable?]]))

(def ^:dynamic *occurs-check* true)

(declare unify)

(defn occur? [sym x bindings]
  (cond
    (nil? x) false
    (= sym x) true
    (and (variable? x) (find bindings x)) (occur? sym (get bindings x) bindings)
    (sequential? x) (or (occur? sym (first x) bindings)
                        (occur? sym (cdr x) bindings))))

(defn unify-variable [sym x bindings]
  (cond
    (find bindings sym) (unify (sym bindings) x bindings)
    (and (variable? x) (find bindings x)) (unify sym (x bindings) bindings)
    (and *occurs-check* (occur? sym x bindings)) nil
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
