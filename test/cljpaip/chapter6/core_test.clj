(ns cljpaip.chapter6.core-test
  (:require
   [clojure.test :as t]
   [cljpaip.chapter6.core :as sut]))

(t/deftest test-literal-match
  (t/is (= 1 1))
  (t/is (= {} (sut/pat-match 42 42)))
  (t/is (= sut/fail (sut/pat-match 42 43)))
  (t/is (= {:?x 42} (sut/pat-match :?x 42))))

(t/deftest test-sequential-pattern-match
  (t/is (= {} (sut/pat-match [42 43] [42 43])))
  (t/is (= {:?x 42} (sut/pat-match [:?x 43] [42 43])))
  (t/is (= sut/fail (sut/pat-match [:?x 43] [42 44])))
  (t/is (= {:?x 42} (sut/pat-match [[:?x] 43] [[42] 43]))))

(t/deftest test-segment-patterns-*
  (t/is (= {:?x []} (sut/pat-match [[:?* :?x] 42] [42])))
  (t/is (= {:?x [7]} (sut/pat-match [[:?* :?x] 42] [7 42])))
  (t/is (= {:?x [7 8]} (sut/pat-match [[:?* :?x] 42] [7 8 42])))
  (t/is (= sut/fail (sut/pat-match [[:?* :?x] 42] [7 8 43]))))

(t/deftest test-segment-patterns-+
  (t/is (= sut/fail (sut/pat-match [[:?+ :?x] 42] [42])))
  (t/is (= {:?x [7]} (sut/pat-match [[:?+ :?x] 42] [7 42])))
  (t/is (= {:?x [7 8]} (sut/pat-match [[:?+ :?x] 42] [7 8 42])))
  (t/is (= sut/fail (sut/pat-match [[:?+ :?x] 42] [7 8 43]))))

(t/deftest test-logical-patterns
  (t/is (= {} (sut/pat-match [:?and 42 42] 42)))
  (t/is (= sut/fail (sut/pat-match [:?and 42 43] 42)))

  (t/is (= {} (sut/pat-match [:?or 42 43] 42)))
  (t/is (= {} (sut/pat-match [:?or 42 43] 43)))
  (t/is (= sut/fail (sut/pat-match [:?or 42 43] 44)))

  (t/is (= {} (sut/pat-match [:?not 43] 42)))
  (t/is (= sut/fail (sut/pat-match [:?not 42] 42))))
