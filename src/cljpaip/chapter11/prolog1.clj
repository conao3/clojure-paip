(ns cljpaip.chapter11.prolog1
  (:require
   [cljpaip.chapter6.pat-match :refer [variable?]]
   [cljpaip.chapter6.eliza :refer [sublis]]
   [cljpaip.chapter11.unify :refer [unify subst-bindings]]))

(declare prove-all)

(def db-predicates (atom nil))

(defn clause-head [clause]
  (first clause))

(defn clause-body [clause]
  (rest clause))

(defn get-clauses [pred]
  (get @db-predicates pred))

(defn predicate [relation]
  (first relation))

(defn add-clause [clause]
  (let [pred (predicate (clause-head clause))]
    (swap! db-predicates update pred conj clause)
    pred))

(defn clear-db []
  (reset! db-predicates nil))

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
          (get-clauses (predicate goal))))

(defn prove-all [goals bindings]
  (cond
    (nil? bindings) nil
    (or (nil? goals) (empty? goals)) [bindings]
    :else (mapcat (fn [goal1-solution]
                    (prove-all (next goals) goal1-solution))
                  (prove (first goals) bindings))))

(defn show-prolog-vars [vars bindings]
  (if (empty? vars)
    (print "\nYes")
    (doseq [var vars]
      (print (str "\n" var " = " (subst-bindings bindings var)))))
  (print ";"))

(defn show-prolog-solutions [vars solutions]
  (if (empty? solutions)
    (println "\nNo.")
    (doseq [solution solutions]
      (show-prolog-vars vars solution))))

(defn top-level-prove [goals]
  (show-prolog-solutions
    (variables-in goals)
    (prove-all goals {})))

(defmacro <- [& clause]
  `(add-clause '~clause))

(defmacro ?- [& goals]
  `(top-level-prove '~goals))
