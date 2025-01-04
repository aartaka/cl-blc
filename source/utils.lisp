;;;; SPDX-FileCopyrightText: Artyom Bologov
;;;; SPDX-License-Identifier: BSD-2 Clause

(in-package :cl-blc)

(defun lambda-p (term)
  (and (listp term)
       (eq 'λ (first term))))

(defmacro define-generic (name (&rest args) &body (documentation . body))
  (assert (stringp documentation))
  `(defgeneric ,name (,@(mapcar #'first (mapcar #'uiop:ensure-list args)))
     (:documentation ,documentation)
     (:method (,@args)
       ,@body)))

(defmacro deftermgeneric (name (term-var &rest args)
                          documentation integer-case &body list-case)
  "Abstract away the frequent pattern of
- Terminate the computation on INTEGER-CASE.
- And do something useful on lists.

Also forces the docs, which is a virtue"
  `(defgeneric ,name (,term-var ,@args)
     (:documentation ,documentation)
     (:method ((,term-var integer) ,@args)
       ,integer-case)
     (:method ((,term-var list) ,@args)
       (progn
         ,@list-case))))

(defmacro destructuring-when ((&rest bindings) form &body body)
  "Run BODY with BINDINGS to FORM result or return NIL when not matching."
  `(ignore-errors
    (destructuring-bind (,@bindings)
        ,form
      ,@body)))

(defun body (term)
  (car (last term)))

(defun tree-transform-if (predicate transformer tree &optional (depth 0))
  (typecase tree
    (integer (if (funcall predicate tree depth)
                 (funcall transformer tree depth)
                 tree))
    (list
     (or
      (when (funcall predicate tree depth)
        (funcall transformer tree depth))
      (if (lambda-p tree)
          (list 'λ (tree-transform-if predicate transformer (body tree) (1+ depth)))
          (list
           (tree-transform-if predicate transformer (first tree) depth)
           (tree-transform-if predicate transformer (second tree) depth)))))))

(defun tree-find-if (predicate tree)
  (block find
    (tree-transform-if
     (lambda (x depth)
       (declare (ignorable depth))
       (funcall predicate x))
     (lambda (x depth)
       (declare (ignorable depth))
       (return-from find x))
     tree)
    nil))

(defun closed-p (term &optional (depth 0))
  (typecase term
    (integer
     (< term depth))
    (list
     (cond
       ((lambda-p term)
        (closed-p (body term) (1+ depth)))
       (t
        (and (closed-p (first term) depth)
             (closed-p (second term) depth)))))))
