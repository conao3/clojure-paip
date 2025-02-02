(ns cljpaip.chapter11.unify
  (:require
   [cljpaip.chapter6.pat-match :refer [variable?]]))

(def ^:dynamic *occurs-check*
  "Should we do the occurs check?"
  true)

(declare unify)

(defn subst-bindings [bindings x]
  (cond
    (nil? bindings) nil
    (= {} bindings) x
    (and (variable? x) (find bindings x)) (subst-bindings bindings (x bindings))
    (not (sequential? x)) x
    :else (cons (subst-bindings bindings (first x))
                (subst-bindings bindings (next x)))))

(defn occur? [sym x bindings]
  (cond
    (nil? x) false
    (= sym x) true
    (and (variable? x) (find bindings x)) (occur? sym (find bindings x) bindings)
    (sequential? x) (or (occur? sym (first x) bindings)
                        (occur? sym (next x) bindings))))

(defn unify-variable [sym x bindings]
  (cond
    (find bindings sym) (unify (sym bindings) x bindings)
    (and (variable? x) (find bindings x)) (unify sym (x bindings) bindings)
    (and *occurs-check* (occur? sym x bindings)) nil
    :else (assoc bindings sym x)))

(defn unify
  ([x y]
   (unify x y {}))
  ([x_ y_ bindings]
   (let [;; when pattern is `(. ?x)`, convert into ?x.
         ;; care about notation of cons-cell.
         peal-cdr-fn #(cond-> %
                        (and (seq? %) (= '. (first %)))
                        ((fn [a]
                           (when (not= 2 (count a))
                             (throw (ex-info "exact 1 element after `.'" {})))
                           (second a))))
         x (peal-cdr-fn x_)
         y (peal-cdr-fn y_)]
     (cond
       (nil? bindings) nil
       (= x y) bindings
       (variable? x) (unify-variable x y bindings)
       (variable? y) (unify-variable y x bindings)
       (and (sequential? x) (sequential? y))
       (unify (next x) (next y)
              (unify (first x) (first y) bindings))))))

(defn unifier [x y]
  (subst-bindings (unify x y) x))
