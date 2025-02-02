(ns cljpaip.chapter11.prolog1-test
  (:require
   [clojure.test :as t]
   [cljpaip.chapter11.prolog1 :as sut]))

(t/deftest variables-in-test
  (t/is (= 1 1))
  (t/is (nil? (sut/variables-in '((member 2 (1 2 3))))))
  (t/is (= '[?x]
           (sut/variables-in '((member ?x (1 2 3))))))
  (t/is (= '[?x ?y]
           (sut/variables-in '((member ?x (1 ?y 3))))))
  (t/is (= '[?x ?y]
           (sut/variables-in '((member ?x (1 ?y ?y)))))))

(t/deftest ?-test
  (do
    (sut/clear-db)
    (sut/<- (member ?item (?x . ?rest)) (member ?item ?rest))
    (sut/<- (member ?item (?item . ?rest))))
  (t/is (= "\nYes;" (with-out-str (sut/?- (member 2 (1 2 3))))))
  (t/is (= "\nYes;\nYes;" (with-out-str (sut/?- (member 2 (1 2 3 2 1))))))
  (t/is (= "\n?x = 1;\n?x = 2;\n?x = 3;" (with-out-str (sut/?- (member ?x (1 2 3)))))))
