;;;; SPDX-FileCopyrightText: Artyom Bologov
;;;; SPDX-License-Identifier: BSD-2 Clause

(uiop:define-package :cl-blc
  (:nicknames :blc)
  (:use :common-lisp)
  (:shadow #:read
           #:optimize
           #:eval
           #:apply
           #:compile
           #:coerce
           #:write
           #:print
           #:princ)
  (:export #:read
           #:eval
           #:compile
           #:coerce
           #:write
           #:print
           #:princ)
  (:documentation "`cl-blc' exports symbols shadowing standard CL.
These symbols implement the logic similar to CL ones, but for Binary
Lambda Calculus use-cases:
- `read' reads a valid BLC from anywhere to IR.
- `eval' evaluates (reduces, technically) the given IR.
- `compile' converts Lisp form/data into IR.
- `coerce' converts IR back to Lisp (effectively reversing `compile').
- `write', `print', and `princ' allow printing the given IR with a
  range of parameters.

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

See the docstrings of the exported functions for usage examples."))
