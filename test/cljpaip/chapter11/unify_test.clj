(ns cljpaip.chapter11.unify-test
  (:require
   [clojure.test :as t]
   [cljpaip.chapter11.unify :as sut]))

(t/deftest unify-test
  (t/is (= {'?x 2, '?y 1} (sut/unify '(?x + 1) '(2 + ?y))))
  (t/is (= {'?x '?y} (sut/unify '?x '?y)))
  (t/is (= {'?x '?y} (sut/unify '(?x ?x) '(?y ?y))))
  (t/is (= {'?x '?y} (sut/unify '(?x ?x ?x) '(?y ?y ?y))))
  (t/is (= {'?x '?y} (sut/unify '(?x ?y) '(?y ?x))))
  (t/is (= {'?x '?y, '?y 'a} (sut/unify '(?x ?y a) '(?y ?x ?x))))
  (t/is (nil? (sut/unify '?x '(f ?x))))
  ;; (t/is (nil? (sut/unify '(?x ?y) '((f ?y) (f ?x))))) ;-> stack overflow
  ;; (t/is (nil? (sut/unify '(?x ?y ?z) '((?y ?z) (?x ?z) (?x ?y))))) ;-> stack overflow
  (t/is (= {} (sut/unify 'a 'a))))

(t/deftest unifier-test
  (t/is (= '(0 + 0 = 0) (sut/unifier '(?a + ?a = 0) '(?x + ?y = ?y))))
  (t/is (= '(2 + 2 = 2) (sut/unifier '(?a + ?a = 2) '(?x + ?y = ?y))))
  (t/is (= '(a a a) (sut/unifier '(?x ?y a) '(?y ?x ?x))))
  (t/is (= '((?a * 5 = 2) + (4 * 5) + 3) (sut/unifier '((?a * ?x = 2) + (?b * ?x) + ?c) '(?z + (4 * 5) + 3)))))

(t/deftest occurs-check-disabled-test
  (binding [sut/*occurs-check* false]
    (t/is (= '{?x (f ?x)} (sut/unify '?x '(f ?x))))
    (t/is (= '{?x (f ?y) ?y (f ?x)} (sut/unify '(?x ?y) '((f ?y) (f ?x)))))
    (t/is (= '{?x (?y ?z) ?y (?x ?z) ?z (?x ?y)} (sut/unify '(?x ?y ?z) '((?y ?z) (?x ?z) (?x ?y)))))))

(t/deftest cons-unify-test
  (t/is (= '{?a 2 ?b 1 ?c (2 3)}
           (sut/unify '(member 2 (1 2 3)) '(member ?a (?b . ?c))))))

(t/deftest member-test
  (t/is (= '{?y 2}
           (sut/unify '(member 2) '(member ?y))))
  (t/is (= '{?y 2, ?z (?x)}
           (sut/unify '(member 2 ?x) '(member ?y . ?z))))

  (t/is (nil? (sut/occur? '?x '(?y . ?z) '{?y 2})))
  (t/is (= '{?y 2 ?x (?y . ?z)}
           (sut/unify '(member 2 ?x) '(member ?y (?y . ?z))))))

(t/deftest nil-test
  (t/is (= {} (sut/unify '() nil))))
