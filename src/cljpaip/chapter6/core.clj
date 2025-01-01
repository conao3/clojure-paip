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

(declare pat-match)

(defmacro trap
  ([body]
   `(trap ~body nil))
  ([body fallback]
   `(try
      ~body
      (catch Exception _#
        ~fallback))))

(defn variable? [x]
  (some? ((fnil re-find nil "") #"^\?" (trap (name x)))))

(defn match-variable [var input bindings]
  (if-let [bound (bindings var)]
    (if (= bound input)
      bindings
      fail)
    (assoc bindings (-> var name (subs 1) keyword) input)))

;;; segment-match

(defn segment-matcher-* [pattern input bindings]
  (let [segment-var (second (first pattern))
        rest-pattern (rest pattern)]
    (loop [start 0]
      (if (> start (count input))
        fail
        (let [prefix (take start input)
              suffix (drop start input)
              new-bindings (pat-match rest-pattern suffix
                                       (match-variable segment-var prefix bindings))]
          (if (= new-bindings fail)
            (recur (inc start))
            new-bindings))))))

(defn segment-matcher-+ [pattern input bindings]
  (let [segment-var (second (first pattern))
        rest-pattern (rest pattern)]
    (loop [start 1]
      (if (> start (count input))
        fail
        (let [prefix (take start input)
              suffix (drop start input)
              new-bindings (pat-match rest-pattern suffix
                                       (match-variable segment-var prefix bindings))]
          (if (= new-bindings fail)
            (recur (inc start))
            new-bindings))))))

(defn segment-match-if [pattern input bindings]
  (let [body (second (first pattern))
        rest-pattern (rest pattern)]
    (if (eval `(let ~(vec (mapcat
                           (fn [[k v]] [(symbol (str "?" (name k))) v])
                           bindings))
                 ~body))
      (pat-match rest-pattern input bindings)
      fail)))

(def segment-match {'?* segment-matcher-*
                    '?+ segment-matcher-+
                    '?if segment-match-if})

(defn segment-match-fn [pattern]
  (get segment-match (first (first pattern))))

(defn segment-pattern? [pattern]
  (and (sequential? pattern)
       (sequential? (first pattern))
       (some? (segment-match-fn pattern))))

(defn segment-matcher [pattern input bindings]
  ((segment-match-fn pattern) pattern input bindings))

;;; single-match

(defn match-is [args input bindings]
  (let [var (first args)
        pred (resolve (second args))]
    (if (pred input)
      (pat-match var input bindings)
      fail)))

(defn match-and [args input bindings]
  (if (= [] args)
    bindings
    (match-and (rest args) input (pat-match (first args) input bindings))))

(defn match-or [args input bindings]
  (if (= [] args)
    fail
    (let [new-bindings (pat-match (first args) input bindings)]
      (if (= new-bindings fail)
        (match-or (rest args) input bindings)
        new-bindings))))

(defn match-not [patterns input bindings]
  (if (match-or patterns input bindings)
    fail
    bindings))

(def single-match {'?is match-is
                   '?and match-and
                   '?or match-or
                   '?not match-not})

(defn single-match-fn [pattern]
  (get single-match (first pattern)))

(defn single-pattern? [pattern]
  (and (sequential? pattern)
       (some? (single-match-fn pattern))))

(defn single-matcher [pattern input bindings]
  ((single-match-fn pattern) (rest pattern) input bindings))

;;; main

(defn pat-match
  ([pattern input]
   (pat-match pattern input no-bindings))
  ([pattern input bindings]
   (cond
     (= bindings fail) fail
     (variable? pattern) (match-variable pattern input bindings)
     (= pattern input) bindings
     (segment-pattern? pattern) (segment-matcher pattern input bindings)
     (single-pattern? pattern) (single-matcher pattern input bindings)
     (and (sequential? pattern) (sequential? input))
     (pat-match (rest pattern) (rest input)
                (pat-match (first pattern) (first input) bindings))
     :else fail)))

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
