(ns cljpaip.chapter11.prolog-test
  (:require
   [clojure.string :as str]
   [clojure.test :as t]
   [cljpaip.chapter11.prolog :as sut]))

(defn lines [& args]
  (str (str/join "\n" args) "\n"))

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

  (t/is (= (lines "Yes" "No.")
           (with-out-str
             (with-in-str ";"
               (sut/?- (member 2 (1 2 3)))))))

  (t/is (= (lines "?list = (2 . ?rest1)"
                  "?list = (?x5 . (2 . ?rest6))"
                  "?list = (?x5 . (?x10 . (2 . ?rest11)))"
                  "No.")
           (let [gensym-cnt (atom 0)]
             (with-redefs [gensym #(-> % (str (swap! gensym-cnt inc)) symbol)]
               (with-out-str
                 (with-in-str (lines ";" ";" ".")
                   (sut/?- (member 2 ?list))))))))

  (t/is (= (lines "?item = ?item2"
                  "?list = (?item2 . ?rest1)"
                  "?item = ?item7"
                  "?list = (?x5 . (?item7 . ?rest6))"
                  "?item = ?item12"
                  "?list = (?x5 . (?x10 . (?item12 . ?rest11)))"
                  "No.")
           (let [gensym-cnt (atom 0)]
             (with-redefs [gensym #(-> % (str (swap! gensym-cnt inc)) symbol)]
               (with-out-str
                 (with-in-str (lines ";" ";" ".")
                   (sut/?- (member ?item ?list))))))))

  (t/is (= (lines "?n = (1 + (1 + (1 + (1 + 0))))" "No.")
           (with-out-str
             (with-in-str (lines ";" ".")
               (sut/?- (length (a b c d) ?n))))))

  (t/is (= (lines "?list = (?x2 . (?x5 . (nil)))" "No.")
           (let [gensym-cnt (atom 0)]
             (with-redefs [gensym #(-> % (str (swap! gensym-cnt inc)) symbol)]
               (with-out-str
                 (with-in-str (lines ";" ".")
                   (sut/?- (length ?list (1 + (1 + 0))))))))))

  (t/is (= (lines "?l = (?x2 . (?x5 . (nil)))" "No.")
           (let [gensym-cnt (atom 0)]
             (with-redefs [gensym #(-> % (str (swap! gensym-cnt inc)) symbol)]
               (with-out-str
                 (with-in-str (lines ";" ".")
                   (sut/?- (length ?l (1 + (1 + 0)))))))))))
