;;;; SPDX-FileCopyrightText: Artyom Bologov
;;;; SPDX-License-Identifier: BSD-2 Clause

(uiop:define-package :cl-blc
  (:nicknames :blc)
  (:use :common-lisp)
  (:shadow #:read
           #:eval
           #:replace
           #:apply
           #:compile
           #:coerce
           #:write
           #:print
           #:princ
           ;; TODO
           #:run ;; On user/file input
           )
  (:documentation "Describe `cl-blc' package here"))
