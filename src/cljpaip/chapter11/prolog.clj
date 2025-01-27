(ns cljpaip.chapter11.prolog
  (:require
   [cljpaip.chapter6.pat-match :refer [variable?]]
   [cljpaip.chapter6.eliza :refer [sublis]]
   [cljpaip.chapter11.unify :refer [unify]]))

(declare prove-all)

(def db-predicates
  "A list of all predicates stored in the database."
  (atom nil))

(defn clause-head [& args]
  (apply first args))

(defn clause-body [& args]
  (apply rest args))

(defn predicate [& args]
  (apply first args))

(defmacro <- [& clause]
  `(add-clause '~clause))

(defmacro ?- [& goals]
  `(prove-all '~goals nil))

(defn add-clause [clause]
  (let [pred-raw (predicate (clause-head clause))
        pred {:clauses (conj (:clauses pred-raw) clause)}]
    (swap! db-predicates conj pred)
    pred))

(defn clear-db []
  (reset! db-predicates nil))

(defn unique-find-anywhere-if
  ([predicate tree]
   (unique-find-anywhere-if predicate tree nil))
  ([predicate tree found-so-far]
   (if (ifn? tree)
     (if (predicate tree)
       (conj found-so-far tree)
       found-so-far)
     (recur predicate (first tree)
            (unique-find-anywhere-if predicate (next tree) found-so-far)))))

(defn variables-in [exp]
  (unique-find-anywhere-if variable? exp))

(defn rename-variables [x]
  (sublis (->> (variables-in x)
               (map (fn [var] [var (gensym (str var))]))
               (into {}))
          x))

(defn prove [goal bindings]
  (mapcat (fn [clause]
            (let [new-clause (rename-variables clause)]
              (prove-all (clause-body new-clause)
                         (unify goal (clause-head new-clause) bindings))))
          (:clauses (predicate goal))))

(defn prove-all [goals bindings]
  (cond
    (nil? bindings) nil
    (nil? goals) [bindings]
    :else (mapcat (fn [goal1-solution]
                    (prove-all (next goals) goal1-solution))
                  (prove (first goals) bindings))))
