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

(defmacro destructuring-try ((&rest bindings) form &body body)
  "Run BODY with BINDINGS to FORM result or return NIL when not matching."
  `(ignore-errors
    (destructuring-bind (,@bindings)
        ,form
      ,@body)))
