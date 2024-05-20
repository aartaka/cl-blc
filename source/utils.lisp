;;;; SPDX-FileCopyrightText: Artyom Bologov
;;;; SPDX-License-Identifier: BSD-2 Clause

(in-package :cl-blc)

(defun lambda-p (term)
  (and (listp term)
       (eq 'λ (first term))))

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

(defun tree-transform-if (predicate transformer tree &optional (depth 0))
  (let ((to-subst nil))
    (labels ((tree-find-if (predicate tree depth)
               (typecase tree
                 (integer (when (funcall predicate tree depth)
                            (push tree to-subst)))
                 (list
                  (or
                   (when (funcall predicate tree depth)
                     (push (list tree depth) to-subst))
                   (if (lambda-p tree)
                       (tree-find-if predicate (second tree) (1+ depth))
                       (progn
                         (tree-find-if predicate (first tree) depth)
                         (tree-find-if predicate (second tree) depth)
                         t)))))))
      (tree-find-if predicate tree depth))
    (reduce (lambda (acc form-to-subst)
              (nsubst (funcall transformer (first form-to-subst) (second form-to-subst))
                      (first form-to-subst) acc))
            to-subst
            :initial-value tree)))

(defun tree-find-if (predicate tree)
  (block find
    (tree-transform-if
     (lambda (x depth)
       (declare (ignorable depth))
       (funcall predicate x))
     (lambda (x depth)
       (declare (ignorable depth))
       (return-from find x))
     tree)))

(defun closed-p (term &optional (depth 0))
  (typecase term
    (integer
     (< term depth))
    (list
     (cond
       ((lambda-p term)
        (closed-p (second term) (1+ depth)))
       (t
        (and (closed-p (first term) depth)
             (closed-p (second term) depth)))))))
