(ns cljpaip.simple.pat-match-test
  (:require
   [clojure.test :as t]
   [cljpaip.chapter6.simple-pat-match :as sut]))

(t/deftest test-literal-match
  (t/is (= 1 1))
  (t/is (= {} (sut/pat-match 42 42)))
  (t/is (nil? (sut/pat-match 42 43)))
  (t/is (= '{?x 42} (sut/pat-match '?x 42))))

(t/deftest test-sequential-pattern-match
  (t/is (= {} (sut/pat-match [42 43] [42 43])))
  (t/is (= '{?x 42} (sut/pat-match '[?x 43] [42 43])))
  (t/is (nil? (sut/pat-match '[?x 43] [42 44])))
  (t/is (= '{?x 42} (sut/pat-match '[[?x] 43] [[42] 43]))))
