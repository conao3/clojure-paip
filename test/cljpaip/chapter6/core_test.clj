(ns cljpaip.chapter6.core-test
  (:require
   [clojure.test :as t]
   [cljpaip.chapter6.core :as sut]))

(t/deftest test-literal-match
  (t/is (= 1 1))
  (t/is (= {} (sut/pat-match 42 42)))
  (t/is (= sut/fail (sut/pat-match 42 43)))
  (t/is (= {:x 42} (sut/pat-match :?x 42))))

(t/deftest test-sequential-pattern-match
  (t/is (= {} (sut/pat-match '(42 43) '(42 43))))
  (t/is (= {:x 42} (sut/pat-match '(?x 43) '(42 43))))
  (t/is (= sut/fail (sut/pat-match '(?x 43) '(42 44))))
  (t/is (= {:x 42} (sut/pat-match '((?x) 43) '((42) 43)))))

(t/deftest test-segment-patterns-*
  (t/is (= {:x []} (sut/pat-match '((?* ?x) 42) '(42))))
  (t/is (= {:x [7]} (sut/pat-match '((?* ?x) 42) '(7 42))))
  (t/is (= {:x [7 8]} (sut/pat-match '((?* ?x) 42) '(7 8 42))))
  (t/is (= sut/fail (sut/pat-match '((?* ?x) 42) '(7 8 43)))))

(t/deftest test-segment-patterns-+
  (t/is (= sut/fail (sut/pat-match '((?+ ?x) 42) '(42))))
  (t/is (= {:x [7]} (sut/pat-match '((?+ ?x) 42) '(7 42))))
  (t/is (= {:x [7 8]} (sut/pat-match '((?+ ?x) 42) '(7 8 42))))
  (t/is (= sut/fail (sut/pat-match '((?+ ?x) 42) '(7 8 43)))))

(t/deftest test-logical-patterns
  (t/is (= {} (sut/pat-match '(?and 42 42) 42)))
  (t/is (= sut/fail (sut/pat-match '(?and 42 43) 42)))

  (t/is (= {} (sut/pat-match '(?or 42 43) 42)))
  (t/is (= {} (sut/pat-match '(?or 42 43) 43)))
  (t/is (= sut/fail (sut/pat-match '(?or 42 43) 44)))

  (t/is (= {} (sut/pat-match '(?not 43) 42)))
  (t/is (= sut/fail (sut/pat-match '(?not 42) 42))))

(t/deftest test-is-pattern
  (t/is (= {:x 42} (sut/pat-match '(?is ?x even?) 42)))
  (t/is (= sut/fail (sut/pat-match '(?is ?x even?) 43)))
  (t/is (= {:x 43} (sut/pat-match '(?is ?x odd?) 43)))
  (t/is (= sut/fail (sut/pat-match '(?is ?x odd?) 42)))

  (t/is (= {:n 34} (sut/pat-match '(x = ?n) '(x = 34))))
  (t/is (= {:n 34} (sut/pat-match '(x = (?is ?n number?)) '(x = 34))))
  (t/is (= sut/fail (sut/pat-match '(x = (?is ?n string?)) '(x = 34)))))

(t/deftest test-if-pattern
  (t/is (= {:x 4 :y 3}
           (sut/pat-match '(?x > ?y (?if (> ?x ?y))) '(4 > 3))))

  (t/is (= sut/fail
           (sut/pat-match '(?x > ?y (?if (> ?x ?y))) '(3 > 4))))

  (t/is (= {:x 5}
           (sut/pat-match '(?x (?if (> ?x 3)) (?if (< ?x 6))) '(5)))))
