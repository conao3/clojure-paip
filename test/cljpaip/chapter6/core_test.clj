(ns cljpaip.chapter6.core-test
  (:require
   [clojure.test :as t]
   [cljpaip.chapter6.core :as sut]))

(t/deftest pat-match-test
  (t/is (= 1 1))
  (t/is (= (sut/pat-match 42 42 {}) {}))
  (t/is (= (sut/pat-match 42 43 {}) nil)))
