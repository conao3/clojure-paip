(ns cljpaip.simple.unify-test
  (:require
   [clojure.test :as t]
   [cljpaip.simple.pat-match :refer [pat-match]]
   [cljpaip.simple.unify :refer [unify]]))

(t/deftest test-literal-match
  (t/is (= 1 1))
  (t/is (= {} (unify 42 42)))
  (t/is (nil? (unify 42 43)))
  (t/is (= '{?x 42} (unify '?x 42))))

(t/deftest test-sequential-pattern-match
  (t/is (= {} (unify '(42 43) '(42 43))))
  (t/is (= '{?x 42} (unify '(?x 43) '(42 43))))
  (t/is (nil? (unify '(?x 43) '(42 44))))
  (t/is (= '{?x 42} (unify '((?x) 43) '((42) 43)))))

(t/deftest test-reverse-match
  (let [a '(?x + 1)
        b '(2 + ?y)]
    (t/is (nil? (pat-match a b)))
    (t/is (= '{?x 2 ?y 1} (unify a b))))

  (let [a '(?x ?y 1)
        b '(?y ?x ?x)]
    (t/is (nil? (pat-match a b)))
    (t/is (= '{?x ?y ?y 1} a b))))

(t/deftest test-unify-variable
  (t/is (= '{?x ?y ?y ?x} (pat-match '(?x ?y) '(?y ?x))))
  (t/is (= '{?x ?y} (unify '(?x ?y) '(?y ?x)))))

(t/deftest unify-test
  (t/is (= '{?x 2 ?y 1} (unify '(?x + 1) '(2 + ?y))))
  (t/is (= '{?x ?y} (unify '?x '?y)))
  (t/is (= '{?x ?y} (unify '(?x ?x) '(?y ?y))))
  (t/is (= '{?x ?y} (unify '(?x ?x ?x) '(?y ?y ?y))))
  (t/is (= '{?x ?y} (unify '(?x ?y) '(?y ?x))))
  (t/is (= '{?x ?y ?y a} (unify '(?x ?y a) '(?y ?x ?x))))
  (t/is (nil? (unify '?x '(f ?x))))
  ;; (t/is (nil? (unify '(?x ?y) '((f ?y) (f ?x))))) ;-> stack overflow
  ;; (t/is (nil? (unify '(?x ?y ?z) '((?y ?z) (?x ?z) (?x ?y))))) ;-> stack overflow
  (t/is (= {} (unify 'a 'a))))
