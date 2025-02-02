;;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;;; Code from Paradigms of AI Programming
;;;; Copyright (c) 1991 Peter Norvig

;;;; File prolog.lisp: prolog from (11.3), with interactive backtracking.

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

;; (requires "unify") ; does not require "prolog1"

;;;; does not include destructive unification (11.6); see prologc.lisp

;; clauses are represented as (head . body) cons cells
(defun clause-head (clause) (first clause))
(defun clause-body (clause) (rest clause))

;; clauses are stored on the predicate's plist
(defun get-clauses (pred) (get pred 'clauses))
(defun predicate (relation) (first relation))
(defun args (x) "The arguments of a relation" (rest x))

(defvar *db-predicates* nil
  "a list of all predicates stored in the database.")

(defun add-clause (clause)
  "add a clause to the data base, indexed by head's predicate."
  ;; the predicate must be a non-variable symbol.
  (let ((pred (predicate (clause-head clause))))
    (assert (and (symbolp pred) (not (variable-p pred))))
    (pushnew pred *db-predicates*)
    (setf (get pred 'clauses)
          (nconc (get-clauses pred) (list clause)))
    pred))

(defun clear-predicate (predicate)
  "remove the clauses for a single predicate."
  (setf (get predicate 'clauses) nil))

(defun clear-db ()
  "remove all clauses (for all predicates) from the data base."
  (mapc #'clear-predicate *db-predicates*))

(defun find-anywhere-if (predicate tree)
  "does predicate apply to any atom in the tree?"
  (if (atom tree)
      (funcall predicate tree)
      (or (find-anywhere-if predicate (first tree))
          (find-anywhere-if predicate (rest tree)))))

(defun unique-find-anywhere-if (predicate tree
                                &optional found-so-far)
  "return a list of leaves of tree satisfying predicate,
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

(defun non-anon-variable-p (x)
  (and (variable-p x) (not (eq x '?))))

(defun variables-in (exp)
  "Return a list of all the variables in EXP."
  (unique-find-anywhere-if #'non-anon-variable-p exp))

(defun rename-variables (x)
  "replace all variables in x with new ones."
  (sublis (mapcar #'(lambda (var) (cons var (gensym (string var))))
                  (variables-in x))
          x))

(defun prove (goal bindings other-goals &optional (depth 0))
  "Return a list of possible solutions to goal."
  (format t "~&~agoal=~a, bindings=~a, other-goals=~a"
          (make-string depth :initial-element #\Space) goal bindings other-goals)
  (let ((res (let ((clauses (get-clauses (predicate goal))))
               (if (listp clauses)
                   (some
                    #'(lambda (clause)
                        (let ((new-clause (rename-variables clause)))
                          (prove-all
                           (append (clause-body new-clause) other-goals)
                           (unify goal (clause-head new-clause) bindings)
                           (+ 1 depth))))
                    clauses)
                   ;; The predicate's "clauses" can be an atom:
                   ;; a primitive function to call
                   (funcall clauses (rest goal) bindings
                            other-goals)))))
    (format t "~&~ares=~a" (make-string depth :initial-element #\Space) res)
    res))

(defun prove-all (goals bindings &optional (depth 0))
  "Find a solution to the conjunction of goals."
  (format t "~&~agoals=~a, bindings=~a"
          (make-string depth :initial-element #\Space) goals bindings)
  (let ((res (cond ((eq bindings fail) fail)
                   ((null goals) bindings)
                   (t (prove (first goals) bindings (rest goals) (+ 1 depth))))))
    (format t "~&~ares=~a" (make-string depth :initial-element #\Space) res)
    res))

(defun continue-p ()
  "Ask user if we should continue looking for solutions."
  (case (read-char)
    (#\; t)
    (#\. nil)
    (#\newline (continue-p))
    (otherwise
      (format t " Type ; to see more or . to stop")
      (continue-p))))

(defun show-prolog-vars (vars bindings other-goals)
  "Print each variable with its binding.
  Then ask the user if more solutions are desired."
  (if (null vars)
      (format t "~&Yes")
      (dolist (var vars)
        (format t "~&~a = ~a" var
                (subst-bindings bindings var))))
  (if (continue-p)
      fail
      (prove-all other-goals bindings)))

(defun top-level-prove (goals)
  (prove-all `(,@goals (show-prolog-vars ,@(variables-in goals)))
             no-bindings)
  (format t "~&No.")
  (values))

(setf (get 'show-prolog-vars 'clauses) 'show-prolog-vars)

(defun replace-?-vars (exp)
    "Replace any ? within exp with a var of the form ?123."
    (cond ((eq exp '?) (gensym "?"))
	  ((atom exp) exp)
	  (t (reuse-cons (replace-?-vars (first exp))
			 (replace-?-vars (rest exp))
			 exp))))

(defmacro ?- (&rest goals) `(top-level-prove ',(replace-?-vars goals)))

(defmacro <- (&rest clause)
  "add a clause to the data base."
  `(add-clause ',(replace-?-vars clause)))

;;; scratch

(format t "db: ~a~&" *db-predicates*)

(<- (member ?item (?item . ?rest)))
(format t "db: ~a~&" *db-predicates*)
(format t "db.member: ~a~&" (get-clauses 'member))

(<- (member ?item (?x . ?rest)) (member ?item ?rest))
(format t "db: ~a~&" *db-predicates*)
(format t "db.member: ~a~&" (get-clauses 'member))

(?- (member 2 (1 2 3)))
