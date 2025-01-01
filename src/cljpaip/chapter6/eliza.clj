(ns cljpaip.chapter6.eliza
  (:require
   [clojure.walk :as walk]
   [cljpaip.chapter6.pat-match :as pat-match]))

(def eliza-rules
  ['(((?* ?x) hello (?* ?y))
     (How do you do.  Please state your problem.))
   '(((?* ?x) I want (?* ?y))
     (What would it mean if you got ?y)
     (Why do you want ?y)
     (Suppose you got ?y soon))
   '(((?* ?x) if (?* ?y))
     (Do you really think its likely that ?y)
     (Do you wish that ?y)
     (What do you think about ?y)
     (Really-- if ?y))
   '(((?* ?x) no (?* ?y))
     (Why not?)
     (You are being a bit negative)
     (Are you saying "NO" just to be negative?))
   '(((?* ?x) I was (?* ?y))
     (Were you really?)
     (Perhaps I already knew you were ?y)
     (Why do you tell me you were ?y now?))
   '(((?* ?x) I feel (?* ?y))
     (Do you often feel ?y ?))
   '(((?* ?x) I felt (?* ?y))
     (What other feelings do you have?))])

(defn sublis [m lst]
  (walk/postwalk-replace m lst))

(defn use-eliza-rules [input]
  (let [[var text] (->> eliza-rules
                        (some
                         #(when-let [res (pat-match/pat-match (first %) input)]
                            [res (rand-nth (rest %))])))]
    (sublis (update-keys var #(->> % name (str "?") symbol)) text)))

(defn eliza []
  (println (flatten (use-eliza-rules (read))))
  (recur))

;; user> (cljpaip.chapter6.eliza/eliza)
;; > (hello there)
;; (How do you do. Please state your problem.)
;; > (i want to test this program)
;; (Suppose you got to test this problem soon)
;; > (i could see if it works)
;; (Do you wish that it works)
;; > (no not really)
;; (You are being a bit negative)
;; > (no)
;; (Are you saying NO just to be negative?)
;; > (forget it-- i was wondering how general the program is)
;; (Perhaps I already knew you were wondering how general the program is)
;; > (i felt like it)
;; (What other feelings do you have?)
;; > (i feel this is enough)
;; (Do you often feel this is enough ?)
