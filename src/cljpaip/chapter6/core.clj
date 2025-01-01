(ns cljpaip.chapter6.core)

;; > (pat-match '(x = (?is ?n numberp)) '(x = 34)) => ((?n . 34))
;; > (pat-match '(x = (?is ?n numberp)) '(x = x)) => NIL
;; > (pat-match '(?x (?or < = >) ?y) '(3 < 4)) => ((?Y . 4) (?X . 3))
;; > (pat-match '(x = (?and (?is ?n numberp) (?is ?n oddp))) '(x = 3)) => ((?N . 3))
;; > (pat-match '(?x /= (?not ?x)) '(3 /= 4)) => ((?X . 3))
;; > (pat-match '(?x > ?y (?if (> ?x ?y))) '(4 > 3)) => ((?Y . 3) (?X . 4))

;; > (pat-match '(x = ?n) nil) => nil
;; > (pat-match '(x = ?n) '(x = 34)) => ((?n . 34))
;; > (pat-match '(x = 34) '(x = 34) {:a 1}) => {:a 1}

;; The pattern must be either
;;   - variable
;;   - constant
;;   - (generalized) segment pattern
;;   - (generalized) single-element pattern
;;   - cons of two patterns

(def fail nil)
(def no-bindings {})

(defn pat-match [pattern input bindings]
  (if (= pattern input)
    bindings
    fail))

(defn eliza []
  (loop [_ nil]
    (println "eliza>")
    (recur (println (read)))))

(defn lisp []
  (loop [_ nil]
    (println ">")
    (recur (println (eval (read))))))

(defn interactive-interpreter [prompt transformer]
  (loop [_ nil]
    (println prompt)
    (recur (println (transformer (read))))))

(defn lisp-mod1 []
  (interactive-interpreter ">" eval))

(defn eliza-mod1 []
  (interactive-interpreter "eliza>" identity))
