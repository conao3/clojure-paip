(ns cljpaip.chapter11.unify-test
  (:require
   [clojure.test :as t]
   [cljpaip.chapter11.unify :as sut]))

;; (sut/unify '(?x + 1) '(2 + ?y))
;; {?x 2, ?y 1}

;; (sut/unify '(f ?x) '(f ?y))
;; {?x ?y}

;; (sut/unify '(?a + ?a = 0) '(?x + ?y = ?y))
;; {?a ?x, ?x ?y, ?y 0}

;; (sut/unifier '(?a + ?a = 0) '(?x + ?y = ?y))
;; (0 + 0 = 0)

;; (sut/unifier '(?a + ?a = 2) '(?x + ?y = ?y))
;; (2 + 2 = 2)

;; (sut/unify '(?x + 1) '(2 + ?y))
;; {?x 2, ?y 1}

;; (sut/unify '?x '?y)
;; {?x ?y}

;; (sut/unify '(?x ?x) '(?y ?y))
;; {?x ?y}

;; (sut/unify '(?x ?x ?x) '(?y ?y ?y))
;; {?x ?y}

;; (sut/unify '(?x ?y) '(?y ?x))
;; {?x ?y}

;; (sut/unify '(?x ?y a) '(?y ?x ?x))
;; {?x ?y, ?y a}

;; (sut/unify '?x '(f ?x)) ;; skip -> OK
;; nil

;; (sut/unify '(?x ?y a) '(?y ?x ?x))
;; {?x ?y, ?y a}

;; (sut/unify '?x '(f ?x))
;; nil

;; (sut/unify '(?x ?y) '((f ?y) (f ?x))) ;; stack overflow

;; (sut/unify '(?x ?y ?z) '((?y ?z) (?x ?z) (?x ?y))) ;; so

;; (sut/unify 'a 'a)
;; {}

;; (sut/unifier '(?x ?y a) '(?y ?x ?x))
;; (a a a)

;; (sut/unifier '((?a * ?x = 2) + (?b * ?x) + ?c) '(?z + (4 * 5) + 3))
;; ((?a * 5 = 2) + (4 * 5) + 3)

;; (with-bindings {#'sut/*occurs-check* false}
;;   (sut/unify '?x '(f ?x)))
;; {?x (f ?x)}

;; (with-bindings {#'sut/*occurs-check* false}
;;   (sut/unify '(?x ?y) '((f ?y) (f ?x))))
;; {?x (f ?y), ?y (f ?x)}

;; (with-bindings {#'sut/*occurs-check* false}
;;   (sut/unify '(?x ?y ?z) '((?y ?z) (?x ?z) (?x ?y))))
;; {?x (?y ?z), ?y (?x ?z), ?z (?x ?y)}
