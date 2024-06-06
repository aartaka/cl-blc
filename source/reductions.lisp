;;;; SPDX-FileCopyrightText: Artyom Bologov
;;;; SPDX-License-Identifier: BSD-2 Clause

(in-package :cl-blc)

(deftermgeneric
    decrease-indices (term depth)
    "Decrease all the indices in TERM by one.
Useful when inlining the form."
    (if (>= term depth)
        (1- term)
        term)
  (if (eq 'λ (first term))
      (list 'λ (decrease-indices (second term) (1+ depth)))
      (list (decrease-indices (first term) depth)
            (decrease-indices (second term) depth))))

(deftermgeneric
    count-rec (term depth)
    "Count all the occurences of DEPTH ref in TERM."
    (if (= term depth)
        1
        0)
  (if (lambda-p term)
      (count-rec (second term) (1+ depth))
      (+ (count-rec (first term) depth)
         (count-rec (second term) depth))))

(deftermgeneric
    eta+-reduce (term)
    "Try to η-reduce (de-alias) the TERM.
(Λ (X 0)) Y => (X Y)

Also does more powerful extensions of the same idea."
    term
  (or (destructuring-when ((lam body) arg)
                          term
        (when (and (eq lam 'λ)
                   (= 1 (count-rec body 0))
                   (or (integerp arg)
                       (closed-p arg)))
          (eta+-reduce
           (tree-transform-if
            (lambda (x depth)
              (equal x depth))
            (lambda (x depth)
              (declare (ignorable x depth))
              (typecase arg
                (integer (+ arg depth))
                (list arg)))
            body))))
      (when (lambda-p term)
        (list 'λ (eta+-reduce (second term))))
      (list (eta+-reduce (first term))
            (eta+-reduce (second term)))
      term))

(deftermgeneric
    dead-reduce (term)
    "Remove the unused arguments."
    term
  (or (destructuring-when ((lam body) arg)
                          term
        (declare (ignorable arg))
        (when (and (eq lam 'λ)
                   (zerop (count-rec body 0)))
          (dead-reduce (decrease-indices body 0))))
      (when (eq 'λ (first term))
        (list 'λ (dead-reduce (second term))))
      (list (dead-reduce (first term))
            (dead-reduce (second term)))))

;; TODO: Iterative version and hash-table-based env?
(defun %beta-reduce (term &optional env)
  "Recursive/procedural CEK machine reference implementation (Wikipedia.)"
  (typecase term
    (integer (values (elt env term) env))
    (cons
     (cond
       ((lambda-p term)
        (values term env))
       (t
        (multiple-value-bind (fn new-env)
            (%beta-reduce (first term) env)
          (apply fn (%beta-reduce (second term) env) new-env)))))))

(defun apply (fn arg env)
  (%beta-reduce (second fn) (cons arg env)))

(defun plug-env (term env &optional (depth 0))
  (cond
    ((lambda-p term)
     (list 'λ (plug-env (second term) env (1+ depth))))
    ((listp term)
     (list (plug-env (first term) env depth)
           (plug-env (second term) env depth)))
    ((integerp term)
     (if (< term depth)
         term
         (elt env (- term depth))))))

(defun beta-reduce (term)
  (multiple-value-bind (term env)
      (%beta-reduce term)
    (tree-transform-if
     (lambda (x d)
       (declare (ignorable d))
       (closed-p x))
     (lambda (x d)
       (declare (ignorable d))
       (%beta-reduce x))
     (plug-env term env))))
