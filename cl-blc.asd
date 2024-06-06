;;;; SPDX-FileCopyrightText: Artyom Bologov
;;;; SPDX-License-Identifier: BSD-2 Clause

(in-package :asdf)

(defsystem "cl-blc"
  :description "cl-blc is a library/CLI tool to run/process Binary Lambda Calculus."
  :author "Artyom Bologov"
  :homepage "https://github.com/aartaka/cl-blc"
  :bug-tracker "https://github.com/aartaka/cl-blc/issues"
  :source-control (:git "https://github.com/aartaka/cl-blc.git")
  :license  "BSD-2 Clause"
  :version "0.0.0"
  :serial t
  :build-operation "program-op"
  :build-pathname "blc"
  :entry-point "cl-blc::entry-point"
  :components ((:module "cl-blc"
                :pathname "source/"
                :components
                ((:file "package")
                 (:file "utils")
                 (:file "cl-blc")
                 (:file "reductions")
                 (:file "cli"))))
  :in-order-to ((test-op (test-op "cl-blc/tests"))))

(defsystem "cl-blc/tests"
  :depends-on ("cl-blc" "lisp-unit2")
  :serial t
  :pathname "tests/"
  :components ((:file "package")
               (:file "tests"))
  :perform (test-op (op c)
                    (eval-input
                     "(lisp-unit2:run-tests
                       :package :cl-blc/tests
                       :run-contexts #'lisp-unit2:with-summary-context)")))
