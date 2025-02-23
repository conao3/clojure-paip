(ns cljpaip.chapter11.prolog-test
  (:require
   [clojure.test :as t]
   [cljpaip.chapter11.prolog :as sut]))

(t/deftest ?-test
  (do
    (sut/clear-db)
    (let [pred 'show-prolog-vars
          clause sut/show-prolog-vars]
      (swap! sut/db-clauses assoc pred clause))
    (sut/<- (member ?item (?x . ?rest)) (member ?item ?rest))
    (sut/<- (member ?item (?item . ?rest))))

  (t/is (= ""
           (with-out-str
             (with-in-str ";"
               (sut/?- (member 2 (1 2 3))))))))
