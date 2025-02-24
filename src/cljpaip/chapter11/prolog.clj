(ns cljpaip.chapter11.prolog
  (:require
   [clojure.walk :as walk]
   [cljpaip.chapter6.pat-match :refer [variable?]]
   [cljpaip.chapter6.eliza :refer [sublis]]
   [cljpaip.chapter11.unify :refer [cdr unify subst-bindings]]))

(declare prove-all)

(defonce db-predicates (atom #{}))
(defonce db-clauses (atom nil))

(defn add-clause [clause]
  (let [pred (first (first clause))]
    (swap! db-predicates conj pred)
    (swap! db-clauses update pred conj clause)
    pred))

(defn clear-db []
  (reset! db-predicates #{})
  (reset! db-clauses nil))

(defn non-anon-variable-p [x]
  (and (variable? x) (not (= x '?))))

(defn unique-find-anywhere-if
  ([predicate tree]
   (unique-find-anywhere-if predicate tree nil))
  ([predicate tree found-so-far]
   (if (not (seq? tree))
     (if (predicate tree)
       (cond-> found-so-far
         (not (some (partial = tree) found-so-far)) (conj tree))
       found-so-far)
     (recur predicate (first tree)
            (unique-find-anywhere-if predicate (next tree) found-so-far)))))

(defn variables-in [exp]
  (unique-find-anywhere-if non-anon-variable-p exp))

(defn rename-variables [x]
  (sublis (->> (variables-in x)
               (map (fn [var] [var (gensym (str var))]))
               (into {}))
          x))

(defn prove [goal bindings other-goals]
  (let [clauses (get @db-clauses (first goal))]
    (if (or (sequential? clauses) (nil? clauses))
      (some (fn [clause]
              (let [new-clause (rename-variables clause)]
                (prove-all
                 (concat (rest new-clause) other-goals)
                 (unify goal (first new-clause) bindings))))
            clauses)
      (clauses (cdr goal) bindings other-goals))))

(defn prove-all [goals bindings]
  (cond
    (nil? bindings) nil
    (or (nil? goals) (empty? goals)) bindings
    :else (prove (first goals) bindings (cdr goals))))

(defn continue? []
  (case (read-line)
    ";" true
    "." nil
    "" (recur)
    :else (do (println "Type ; to see more or . to stop.")
              (recur))))

(defn show-prolog-vars [vars bindings other-goals]
  (if (empty? vars)
    (print "\nYes")
    (doseq [var vars]
      (print (str "\n" var " = " (subst-bindings bindings var)))))
  (if (continue?)
    nil
    (prove-all other-goals bindings)))

(defn top-level-prove [goals]
  (prove-all `(~@goals (~'show-prolog-vars ~@(variables-in goals))) {})
  (print "\nNo."))

(defn replace-?-vars [exp]
  (->> exp
       (walk/prewalk
        (fn [elm]
          (cond-> elm (= elm '?) ((fn [_] (gensym "?"))))))))

(defmacro <- [& clause]
  `(add-clause '~(replace-?-vars clause)))

(defmacro ?- [& goals]
  `(top-level-prove '~(replace-?-vars goals)))
