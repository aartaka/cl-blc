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
                   (closed-p (list lam body))
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

(deftermgeneric
    neighbor-reduce (term)
    "(Λ ...) Y (non-lambda) => (...[0=Y])"
    term
  (or (destructuring-when ((lam body) arg)
                          term
        (when (and (eq lam 'λ)
                   ;; (integerp arg)
                   (not (lambda-p arg))
                   (not (closed-p (list lam body)))
                   (not (tree-find-if #'lambda-p arg)))
          (tree-transform-if
           (lambda (x depth)
             (and (integerp x)
                  (>= x depth)))
           (lambda (x outer-depth)
             (declare (ignorable x))
             ;; Necessary to downgrade the outer scope catching ones.
             (if (> x outer-depth)
                 (1- x)
                 (tree-transform-if
                  (lambda (x d)
                    (declare (ignorable d))
                    (integerp x))
                  (lambda (x d)
                    (declare (ignorable d))
                    (+ outer-depth x))
                  arg)))
           body)))
      (when (lambda-p term)
        (list 'λ (neighbor-reduce (second term))))
      (list (neighbor-reduce (first term))
            (neighbor-reduce (second term)))))

(defun named-transform (term &optional (env '()))
  (cond
    ((lambda-p term)
     (let ((name (gensym)))
       (list 'λ name (named-transform (body term) (cons name env)))))
    ((integerp term)
     (nth term env))
    (t
     (list (named-transform (first term) env)
           (named-transform (second term) env)))))

(defun unnamed-transform (named-term &optional (env '()))
  (cond
    ((lambda-p named-term)
     (list 'λ (unnamed-transform (body named-term)
                                 (cons (second named-term) env))))
    ((symbolp named-term)
     (position named-term env))
    ((listp named-term)
     (list (unnamed-transform (first named-term) env)
           (unnamed-transform (second named-term) env)))))

(defun %plug-env (named-term env)
  (loop for (symbol . value) in (reverse env)
        for term = (subst value symbol named-term)
          then (subst value symbol term)
        finally (return (or term named-term))))

(defun assoc-env (x env)
  (loop for binding = (cdr (assoc x env))
          then (cdr (assoc binding env))
        until (listp binding)
        finally (return binding)))

(defvar *env* '())
(defun %beta-reduce (named-term)
  (cond
    ((lambda-p named-term)
     (%plug-env named-term *env*))
    ((symbolp named-term)
     (%plug-env (assoc-env named-term *env*) *env*))
    ((listp named-term)
     (let* ((fn (%plug-env (%beta-reduce (first named-term)) *env*))
            (fn (if (symbolp fn)
                    (assoc-env fn *env*)
                    fn))
            (*env* (cons (cons (second fn) (%beta-reduce (second named-term)))
                         *env*)))
       (%beta-reduce (body fn))))))

(defun beta-reduce (term)
  (tree-transform-if
   (lambda (x d)
     (declare (ignorable d))
     (closed-p x))
   (lambda (x d)
     (declare (ignorable d))
     (unnamed-transform
      (%beta-reduce
       (named-transform x))))
   (unnamed-transform
    (%beta-reduce (named-transform term)))))
