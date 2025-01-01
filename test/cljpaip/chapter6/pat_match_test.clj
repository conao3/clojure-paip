(ns cljpaip.chapter6.pat-match-test
  (:require
   [clojure.test :as t]
   [cljpaip.chapter6.pat-match :as sut]))

(t/deftest test-literal-match
  (t/is (= 1 1))
  (t/is (= {} (sut/pat-match 42 42)))
  (t/is (nil? (sut/pat-match 42 43)))
  (t/is (= {:x 42} (sut/pat-match :?x 42))))

(t/deftest test-sequential-pattern-match
  (t/is (= {} (sut/pat-match '(42 43) '(42 43))))
  (t/is (= {:x 42} (sut/pat-match '(?x 43) '(42 43))))
  (t/is (nil? (sut/pat-match '(?x 43) '(42 44))))
  (t/is (= {:x 42} (sut/pat-match '((?x) 43) '((42) 43)))))

(t/deftest test-segment-patterns-*
  (t/is (= {:x []} (sut/pat-match '((?* ?x) 42) '(42))))
  (t/is (= {:x [7]} (sut/pat-match '((?* ?x) 42) '(7 42))))
  (t/is (= {:x [7 8]} (sut/pat-match '((?* ?x) 42) '(7 8 42))))
  (t/is (nil? (sut/pat-match '((?* ?x) 42) '(7 8 43)))))

(t/deftest test-segment-patterns-+
  (t/is (nil? (sut/pat-match '((?+ ?x) 42) '(42))))
  (t/is (= {:x [7]} (sut/pat-match '((?+ ?x) 42) '(7 42))))
  (t/is (= {:x [7 8]} (sut/pat-match '((?+ ?x) 42) '(7 8 42))))
  (t/is (nil? (sut/pat-match '((?+ ?x) 42) '(7 8 43)))))

(t/deftest test-logical-patterns
  (t/is (= {} (sut/pat-match '(?and 42 42) 42)))
  (t/is (nil? (sut/pat-match '(?and 42 43) 42)))

  (t/is (= {} (sut/pat-match '(?or 42 43) 42)))
  (t/is (= {} (sut/pat-match '(?or 42 43) 43)))
  (t/is (nil? (sut/pat-match '(?or 42 43) 44)))

  (t/is (= {} (sut/pat-match '(?not 43) 42)))
  (t/is (nil? (sut/pat-match '(?not 42) 42))))

(t/deftest test-is-pattern
  (t/is (= {:x 42} (sut/pat-match '(?is ?x even?) 42)))
  (t/is (nil? (sut/pat-match '(?is ?x even?) 43)))
  (t/is (= {:x 43} (sut/pat-match '(?is ?x odd?) 43)))
  (t/is (nil? (sut/pat-match '(?is ?x odd?) 42)))

  (t/is (= {:n 34} (sut/pat-match '(x = ?n) '(x = 34))))
  (t/is (= {:n 34} (sut/pat-match '(x = (?is ?n number?)) '(x = 34))))
  (t/is (nil? (sut/pat-match '(x = (?is ?n string?)) '(x = 34)))))

(t/deftest test-if-pattern
  (t/is (= {:x 4 :y 3}
           (sut/pat-match '(?x > ?y (?if (> ?x ?y))) '(4 > 3))))

  (t/is (nil? (sut/pat-match '(?x > ?y (?if (> ?x ?y))) '(3 > 4))))

  (t/is (= {:x 5}
           (sut/pat-match '(?x (?if (> ?x 3)) (?if (< ?x 6))) '(5)))))

(t/deftest test-paip-text
  (t/testing "introduction"
    (t/is (= {:n 34}
             (sut/pat-match '(x = (?is ?n number?)) '(x = 34))))
    (t/is (nil? (sut/pat-match '(x = (?is ?n number?)) '(x = x)))))

  (t/testing "or pattern"
    (t/is (= {:x 3, :y 4}
             (sut/pat-match '(?x (?or < = >) ?y) '(3 < 4)))))

  (t/testing "and pattern"
    (t/is (= {:n 3}
             (sut/pat-match '(x = (?and (?is ?n number?) (?is ?n odd?))) '(x = 3))))
    (t/is (nil? (sut/pat-match '(x = (?and (?is ?n number?) (?is ?n odd?))) '(x = 4)))))

  (t/testing "not pattern"
    (t/is (= {:x 3}
             (sut/pat-match '(?x != (?not ?x)) '(3 != 4))))
    (t/is (nil? (sut/pat-match '(?x != (?not ?x)) '(3 != 3)))))

  (t/testing "* pattern"
    (t/is (= {:x ['b 'c]}
             (sut/pat-match '(a (?* ?x) d) '(a b c d))))

    (t/is (= {:x [] :y ['b 'c]}
             (sut/pat-match '(a (?* ?x) (?* ?y) d) '(a b c d))))

    (t/is (= {:x ['b 'c] :y ['d]}
             (sut/pat-match '(a (?* ?x) (?* ?y) ?x ?y) '(a b c d (b c) (d))))))

  (t/testing "if pattern"
    (t/is (= {:x 3 :op '+ :y 4 :z 7}
             (sut/pat-match '(?x ?op ?y is ?z (?if (= (?op ?x ?y) ?z)))
                            '(3 + 4 is 7))))

    (t/is (nil? (sut/pat-match '(?x ?op ?y (?if (?op ?x ?y)))
                               '(3 > 4))))))
