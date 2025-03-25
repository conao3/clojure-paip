(ns cljpaip.simple.util-test
  (:require
   [clojure.test :as t]
   [cljpaip.simple.util :as sut]))

(t/deftest variable?-test
  (t/is (= true (sut/variable? '?x)))
  (t/is (= false (sut/variable? 'x)))
  (t/is (= false (sut/variable? 1))))
