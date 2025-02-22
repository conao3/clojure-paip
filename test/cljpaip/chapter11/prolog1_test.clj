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
  (t/is (= "\n?x = 1;\n?x = 2;\n?x = 3;" (with-out-str (sut/?- (member ?x (1 2 3))))))

  (do
    (sut/clear-db)
    (sut/<- (likes Kim Robin))
    (sut/<- (likes Sandy Lee))
    (sut/<- (likes Sandy Kim))
    (sut/<- (likes Robin cats))
    (sut/<- (likes Sandy ?x) (likes ?x cats))
    (sut/<- (likes Kim ?x) (likes ?x Lee) (likes ?x Kim))
    (sut/<- (likes ?x ?x)))
  (t/is (= "
?who = Sandy;
?who = cats;
?who = Sandy;
?who = Robin;
?who = Kim;
?who = Lee;" (with-out-str (sut/?- (likes Sandy ?who)))))
  (t/is (= "
?who = Sandy;
?who = Kim;
?who = Sandy;" (with-out-str (sut/?- (likes ?who Sandy)))))
  (t/is (= "\nNo." (with-out-str (sut/?- (likes Robin Lee)))))
  (t/is (= "
?y = ?x37
?x = ?x37;
?y = Sandy
?x = Sandy;
?y = Sandy
?x = Kim;
?y = Sandy
?x = Sandy;
?y = Sandy
?x = Sandy;
?y = Kim
?x = Sandy;"
           (let [gensym-cnt (atom 0)]
             (with-redefs [gensym #(-> % (str (swap! gensym-cnt inc)) symbol)]
               (with-out-str (sut/?- (likes ?x ?y) (likes ?y ?x))))))))
