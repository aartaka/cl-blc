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
    eta-reduce (term)
    "Try to η-reduce (de-alias) the TERM.
(Λ (X 0)) Y => (X Y)"
    term
  (or (destructuring-when ((lam body) arg)
                          term
        (when (and (eq lam 'λ)
                   (= 1 (count-rec body 0)))
          (labels ((subst-zero (thing subst depth)
                     (typecase thing
                       (integer (if (= thing depth)
                                    subst
                                    thing))
                       (list (if (lambda-p thing)
                                 (list 'λ (subst-zero (second thing) subst (1+ depth)))
                                 (list (subst-zero (first thing) subst depth)
                                       (subst-zero (second thing) subst depth)))))))
            (eta-reduce
             (subst-zero body arg 0)))))
      (when (lambda-p term)
        (list 'λ (eta-reduce (second term))))
      (list (eta-reduce (first term))
            (eta-reduce (second term)))
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
    identity-reduce (term)
    "Reduce the TERM if it's just an identity application."
    term
  (cond
    ((equal '(λ 0) (first term))
     (identity-reduce (second term)))
    ((eq 'λ (first term))
     (list 'λ (identity-reduce (second term))))
    (t (list (identity-reduce (first term))
             (identity-reduce (second term))))))
