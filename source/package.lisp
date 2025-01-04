;;;; SPDX-FileCopyrightText: Artyom Bologov
;;;; SPDX-License-Identifier: BSD-2 Clause

(uiop:define-package :cl-blc
  (:nicknames :blc)
  (:use :common-lisp)
  (:shadow #:read
           #:eval
           #:compile
           #:coerce
           #:funcall)
  (:export #:read
           #:eval
           #:compile
           #:coerce)
  (:documentation "`cl-blc' exports symbols shadowing standard CL.
These symbols implement the logic similar to CL ones, but for Binary
Lambda Calculus use-cases:
- `read' reads a valid BLC from anywhere to IR.
- `eval' evaluates the given IR to a compiled function.
- `compile' converts Lisp form/data into IR.
- `coerce' converts IR/evaluation back to Lisp (effectively reversing `compile').

IR is mapped directly from BLC:
- Forms starting with λ are abstractions.
- Lists are applications.
- And numbers are relative argument references.

For instance, IR for TRUE is:
(read \"0000110\")
;; => (Λ (Λ 1)), NIL
(compile '(lambda (x y) x))
;; => (Λ (Λ 1))
(compile t)
;; => (Λ (Λ 1))

See the docstrings of the exported functions for usage examples.
But here's one to tease you:
(coerce (eval (list (read #p\"~/git/cl-blc/example/first.blc\")
                    (compile '('(3 2 1)))))
        'number)
;; => 3"))
