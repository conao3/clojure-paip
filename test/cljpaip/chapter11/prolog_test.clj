(ns cljpaip.chapter11.prolog-test
  (:require
   [clojure.string :as str]
   [clojure.test :as t]
   [cljpaip.chapter11.prolog :as sut]))

(t/deftest ?-test
  (do
    (sut/clear-db)
    (let [pred 'show-prolog-vars
          clause sut/show-prolog-vars]
      (swap! sut/db-clauses assoc pred clause))
    (sut/<- (member ?item (?x . ?rest)) (member ?item ?rest))
    (sut/<- (member ?item (?item . ?rest)))

    (sut/<- (length () 0))
    (sut/<- (length (?x . ?y) (1 + ?n)) (length ?y ?n)))

  (t/is (= "
Yes
No."
           (with-out-str
             (with-in-str ";"
               (sut/?- (member 2 (1 2 3)))))))

  (t/is (= "
?list = (2 . ?rest2)
?list = (?x3 . (2 . ?rest7))
?list = (?x3 . (?x8 . (2 . ?rest12)))
No."
           (let [gensym-cnt (atom 0)]
             (with-redefs [gensym #(-> % (str (swap! gensym-cnt inc)) symbol)]
               (with-out-str
                 (with-in-str (str/join "\n" [";" ";" "."])
                   (sut/?- (member 2 ?list))))))))

  (t/is (= "
?item = ?item1
?list = (?item1 . ?rest2)
?item = ?item6
?list = (?x3 . (?item6 . ?rest7))
?item = ?item11
?list = (?x3 . (?x8 . (?item11 . ?rest12)))
No."
           (let [gensym-cnt (atom 0)]
             (with-redefs [gensym #(-> % (str (swap! gensym-cnt inc)) symbol)]
               (with-out-str
                 (with-in-str (str/join "\n" [";" ";" "."])
                   (sut/?- (member ?item ?list))))))))

  (t/is (= "
?n = (1 + (1 + (1 + (1 + 0))))
No."
           (with-out-str
             (with-in-str (str/join "\n" [";" "."])
               (sut/?- (length (a b c d) ?n))))))

  (t/is (= "
?list = (?x1 . (?x4 . (nil)))
No."
           (let [gensym-cnt (atom 0)]
             (with-redefs [gensym #(-> % (str (swap! gensym-cnt inc)) symbol)]
               (with-out-str
                 (with-in-str (str/join "\n" [";" "."])
                   (sut/?- (length ?list (1 + (1 + 0))))))))))

  (t/is (= "
?l = (?x1 . (?x4 . (nil)))
No."
           (let [gensym-cnt (atom 0)]
             (with-redefs [gensym #(-> % (str (swap! gensym-cnt inc)) symbol)]
               (with-out-str
                 (with-in-str (str/join "\n" [";" "."])
                   (sut/?- (length ?l (1 + (1 + 0)))))))))))
