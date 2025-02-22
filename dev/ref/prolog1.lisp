;;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;;; Code from Paradigms of AI Programming
;;;; Copyright (c) 1991 Peter Norvig

;;;; File prolog1.lisp: First version of the prolog interpreter (11.2).

(defun reuse-cons (x y x-y)
 "Return (cons x y), or just x-y if it is equal to (cons x y)."
 (if (and (eql x (car x-y)) (eql y (cdr x-y)))
   x-y
   (cons x y)))
(defconstant fail nil "Indicates pat-match failure")
(defconstant no-bindings '((t . t))
  "Indicates pat-match success, with no variables.")
(defun variable-p (x)
  "Is x a variable (a symbol beginning with `?')?"
  (and (symbolp x) (equal (elt (symbol-name x) 0) #\?)))
(defun get-binding (var bindings)
 "Find a (variable . value) pair in a binding list."
 (assoc var bindings))
(defun binding-val (binding)
 "Get the value part of a single binding."
 (cdr binding))
(defun lookup (var bindings)
 "Get the value part (for var) from a binding list."
 (binding-val (get-binding var bindings)))
(defun extend-bindings (var val bindings)
 "Add a (var . value) pair to a binding list."
 (cons (cons var val)
       ;; Once we add a "real" binding,
       ;; we can get rid of the dummy no-bindings
       (if (and (eq bindings no-bindings))
           nil
           bindings)))
(defparameter *occurs-check* t "Should we do the occurs check?")
(defun occurs-check (var x bindings)
  "Does var occur anywhere inside x?"
  (cond ((eq var x) t)
        ((and (variable-p x) (get-binding x bindings))
         (occurs-check var (lookup x bindings) bindings))
        ((consp x) (or (occurs-check var (first x) bindings)
                       (occurs-check var (rest x) bindings)))
        (t nil)))
(defun unify-variable (var x bindings)
  "Unify var with x, using (and maybe extending) bindings."
  (cond ((get-binding var bindings)
         (unify (lookup var bindings) x bindings))
        ((and (variable-p x) (get-binding x bindings))
         (unify var (lookup x bindings) bindings))
        ((and *occurs-check* (occurs-check var x bindings))
         fail)
        (t (extend-bindings var x bindings))))
(defun unify (x y &optional (bindings no-bindings))
  "See if x and y match with given bindings."
  (cond ((eq bindings fail) fail)
        ((eql x y) bindings)
        ((variable-p x) (unify-variable x y bindings))
        ((variable-p y) (unify-variable y x bindings))
        ((and (consp x) (consp y))
         (unify (rest x) (rest y)
                (unify (first x) (first y) bindings)))
        (t fail)))
(defun subst-bindings (bindings x)
  "Substitute the value of variables in bindings into x,
  taking recursively bound variables into account."
  (cond ((eq bindings fail) fail)
        ((eq bindings no-bindings) x)
        ((and (variable-p x) (get-binding x bindings))
         (subst-bindings bindings (lookup x bindings)))
        ((atom x) x)
        (t (reuse-cons (subst-bindings bindings (car x))
                       (subst-bindings bindings (cdr x))
                       x))))

;;; main

;; (requires "unify")

;; Clauses are represented as (head . body) cons cells
(defun clause-head (clause) (first clause))
(defun clause-body (clause) (rest clause))

;; Clauses are stored on the predicate's plist
(defun get-clauses (pred) (get pred 'clauses))
(defun predicate (relation) (first relation))

(defvar *db-predicates* nil
  "A list of all predicates stored in the database.")

(defun add-clause (clause)
  "Add a clause to the data base, indexed by head's predicate."
  ;; The predicate must be a non-variable symbol.
  (let ((pred (predicate (clause-head clause))))
    (assert (and (symbolp pred) (not (variable-p pred))))
    (pushnew pred *db-predicates*)
    (setf (get pred 'clauses)
          (nconc (get-clauses pred) (list clause)))
    pred))

(defun clear-predicate (predicate)
  "Remove the clauses for a single predicate."
  (setf (get predicate 'clauses) nil))

(defun clear-db ()
  "Remove all clauses (for all predicates) from the data base."
  (mapc #'clear-predicate *db-predicates*))

(defun find-anywhere-if (predicate tree)
  "Does predicate apply to any atom in the tree?"
  (if (atom tree)
      (funcall predicate tree)
      (or (find-anywhere-if predicate (first tree))
          (find-anywhere-if predicate (rest tree)))))

(defun unique-find-anywhere-if (predicate tree
                                &optional found-so-far)
  "Return a list of leaves of tree satisfying predicate,
  with duplicates removed."
  (if (atom tree)
      (if (funcall predicate tree)
          (adjoin tree found-so-far)
          found-so-far)
      (unique-find-anywhere-if
        predicate
        (first tree)
        (unique-find-anywhere-if predicate (rest tree)
                                 found-so-far))))

(defun variables-in (exp)
  "Return a list of all the variables in EXP."
  (unique-find-anywhere-if #'variable-p exp))

(defun rename-variables (x)
  "Replace all variables in x with new ones."
  (sublis (mapcar #'(lambda (var) (cons var (gensym (string var))))
                  (variables-in x))
          x))

(defun prove (goal bindings)
  "Return a list of possible solutions to goal."
  (mapcan #'(lambda (clause)
              (let ((new-clause (rename-variables clause)))
                (prove-all (clause-body new-clause)
                           (unify goal (clause-head new-clause) bindings))))
          (get-clauses (predicate goal))))

(defun prove-all (goals bindings &optional (depth 0))
  "Return a list of solutions to the conjunction of goals."
  (format t "~agoals=~a bindings=~a~&" (make-string (* depth 2) :initial-element #\Space) goals bindings)
  (let ((res (cond ((eq bindings fail) fail)
                   ((null goals) (list bindings))
                   (t (mapcan #'(lambda (goal1-solution)
                                  (prove-all (rest goals) goal1-solution (+ 1 depth)))
                              (prove (first goals) bindings))))))
    (format t "~ares=~a~&" (make-string (* depth 2) :initial-element #\Space) res)
    res))

(defun show-prolog-vars (vars bindings)
  "Print each variable with its binding."
  (if (null vars)
      (format t "~&Yes")
      (dolist (var vars)
        (format t "~&~a = ~a" var
                (subst-bindings bindings var))))
  (princ ";"))

(defun show-prolog-solutions (vars solutions)
  "Print the variables in each of the solutions."
  (if (null solutions)
      (format t "~&No.")
      (mapc #'(lambda (solution) (show-prolog-vars vars solution))
            solutions))
  (values))

(defun top-level-prove (goals)
  "Prove the goals, and print variables readably."
  (show-prolog-solutions
    (variables-in goals)
    (prove-all goals no-bindings)))

(defmacro <- (&rest clause)
  "Add a clause to the data base."
  `(add-clause ',clause))

(defmacro ?- (&rest goals) `(top-level-prove ',goals))

;;; scratch

(setf *print-pretty* nil)

(format t "db: ~a~&" *db-predicates*)

(<- (member ?item (?item . ?rest)))
(format t "db: ~a~&" *db-predicates*)
(format t "db.member: ~a~&" (get-clauses 'member))

(<- (member ?item (?x . ?rest)) (member ?item ?rest))
(format t "db: ~a~&" *db-predicates*)
(format t "db.member: ~a~&" (get-clauses 'member))

;; (defun prove-all (goals bindings)
;;   "Return a list of solutions to the conjunction of goals."
;;   (cond ((eq bindings fail) fail)
;;         ((null goals) (list bindings))
;;         (t (mapcan #'(lambda (goal1-solution)
;;                        (prove-all (rest goals) goal1-solution))
;;                    (prove (first goals) bindings)))))

;; (defun prove (goal bindings)
;;   "Return a list of possible solutions to goal."
;;   (mapcan #'(lambda (clause)
;;               (let ((new-clause (rename-variables clause)))
;;                 (prove-all (clause-body new-clause)
;;                            (unify goal (clause-head new-clause) bindings))))
;;           (get-clauses (predicate goal))))

(?- (member 2 (1 2 3)))
;; (let ((goals '((member 2 (1 2 3)))))
;;   (print (top-level-prove goals))
;;   (print (show-prolog-solutions
;;           (variables-in goals)
;;           (prove-all goals no-bindings)))
;;   (print (prove-all goals no-bindings))
;;   (let ((bindings no-bindings))
;;     (print (prove (first goals) bindings))
;;     (let ((goal (first goals)))
;;       (print (get-clauses (predicate goal)))
;;       (let ((clause (second (get-clauses (predicate goal)))))
;;         (print clause)
;;         (let ((new-clause (rename-variables clause)))
;;           (print new-clause)
;;           (print (prove-all (clause-body new-clause)
;;                             (unify goal (clause-head new-clause) bindings)))
;;           (let ((goals (clause-body new-clause))
;;                 (bindings (unify goal (clause-head new-clause) bindings)))
;;             (print (get-clauses (predicate goal)))
;;             (let ((clause (second (get-clauses (predicate goal)))))
;;               (let ((new-clause (rename-variables clause)))
;;                 (print (prove-all (clause-body new-clause)
;;                                   (unify goal (clause-head new-clause) bindings)))))))))))
