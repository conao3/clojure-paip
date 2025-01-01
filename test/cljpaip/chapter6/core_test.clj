(ns cljpaip.chapter6.core-test
  (:require
   [clojure.test :as t]
   [cljpaip.chapter6.core :as sut]))

(t/deftest test-literal-match
  (t/is (= 1 1))
  (t/is (= (sut/pat-match 42 42) {}))
  (t/is (= (sut/pat-match 42 43) sut/fail))
  (t/is (= (sut/pat-match :?x 42) {:?x 42})))
