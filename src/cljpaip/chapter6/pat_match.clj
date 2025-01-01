(ns cljpaip.chapter6.pat-match)

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
  (let [k (-> var name (subs 1) keyword)
        binding (find bindings k)]
    (cond (nil? binding) (assoc bindings k input)
          (= (val binding) input) bindings)))

;;; segment-match

(defn segment-matcher-*
  ([pattern input bindings]
   (segment-matcher-* pattern input bindings 0))
  ([pattern input bindings start]
   (let [segment-var (second (first pattern))
         rest-pattern (rest pattern)]
     (when-not (> start (count input))
       (let [prefix (take start input)
             suffix (drop start input)
             new-bindings (pat-match rest-pattern suffix
                                     (match-variable segment-var prefix bindings))]
         (if (nil? new-bindings)
           (recur pattern input bindings (inc start))
           new-bindings))))))

(defn segment-matcher-+ [pattern input bindings]
  (segment-matcher-* pattern input bindings 1))

(defn segment-match-if [pattern input bindings]
  (let [body (second (first pattern))
        rest-pattern (rest pattern)]
    (when (eval `(let ~(vec (mapcat
                             (fn [[k v]] [(symbol (str "?" (name k))) v])
                             bindings))
                   ~body))
      (pat-match rest-pattern input bindings))))

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
    (when (pred input)
      (pat-match var input bindings))))

(defn match-and [args input bindings]
  (if (= [] args)
    bindings
    (match-and (rest args) input (pat-match (first args) input bindings))))

(defn match-or [args input bindings]
  (when-not (= [] args)
    (let [new-bindings (pat-match (first args) input bindings)]
      (if (nil? new-bindings)
        (match-or (rest args) input bindings)
        new-bindings))))

(defn match-not [patterns input bindings]
  (when-not (match-or patterns input bindings)
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
   (pat-match pattern input {}))
  ([pattern input bindings]
   (cond
     (nil? bindings) nil
     (variable? pattern) (match-variable pattern input bindings)
     (= pattern input) bindings
     (segment-pattern? pattern) (segment-matcher pattern input bindings)
     (single-pattern? pattern) (single-matcher pattern input bindings)
     (and (sequential? pattern) (sequential? input))
     (pat-match (rest pattern) (rest input)
                (pat-match (first pattern) (first input) bindings)))))
