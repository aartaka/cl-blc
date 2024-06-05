;;;; SPDX-FileCopyrightText: Artyom Bologov
;;;; SPDX-License-Identifier: BSD-2 Clause

(in-package :cl-blc)

(defgeneric read (data)
  (:documentation "Parse the DATA as source of BLC expressions.
Supports:
- Bitvectors.
 (read #*0000110)
 ;; => (Λ (Λ 1))
- Lists of numbers.
- Unsigned byte arrays.
- Strings.
 (read \"0000110\")
 ;; => (Λ (Λ 1)), NIL
- Streams (both binary and character).
- Pathnames (both true binary and filled with ones and zero chars):
 (read \"/path/to/cl-blc/example/length.blc\")
 ;; => (Λ ((Λ (((Λ (Λ (Λ ((0 2) 1)))) 0) (Λ (Λ 0)))) (0 (Λ (Λ 1)))))"))

(defmethod read ((data bit-vector))
  (read (cl:coerce data 'list)))

(defmethod read ((data pathname))
  (with-open-file (s data
                     :direction :input
                     :element-type '(unsigned-byte 8))
    (read s)))

(defun stream->sequence (stream)
  (if (equal '(unsigned-byte 8)
             (stream-element-type stream))
      (cl:coerce (loop for byte = (read-byte stream nil nil)
                       while byte
                       collect byte)
                 'vector)
      (uiop:slurp-stream-string stream)))

(defmethod read ((data stream))
  (read (stream->sequence data)))

(defmethod integer->bit-list (integer)
  (flet ((bool->integer (bool)
           (if (zerop bool)
               0
               1)))
    (list
     (bool->integer (logand integer #b10000000))
     (bool->integer (logand integer #b01000000))
     (bool->integer (logand integer #b00100000))
     (bool->integer (logand integer #b00010000))
     (bool->integer (logand integer #b00001000))
     (bool->integer (logand integer #b00000100))
     (bool->integer (logand integer #b00000010))
     (bool->integer (logand integer #b00000001)))))

(defmethod read ((data array))
  ;; Heuristic: if there are 20%+ non-ASCII chars, then it's a binary
  ;; file. Imperfect, but will do.
  (read (if (> (count-if (lambda (b)
                           (or (< b 32)
                               (> b 128)))
                         data)
               (/ (length data) 5))
            (loop for byte across data
                  append (integer->bit-list byte))
            (map 'string #'code-char data))))

(defmethod read ((data string))
  (read
   (loop for char across data
         when (eql #\0 char)
           collect 0
         when (eql #\1 char)
           collect 1)))

(defmethod read ((data list))
  (let ((first (first data)))
    (cond
      ((null first)
       (values))
      ((and (zerop first)
            (zerop (second data)))
       (multiple-value-bind (body rest)
           (read (cddr data))
         (values `(λ ,body) rest)))
      ((zerop first)
       (multiple-value-bind (function rest)
           (read (cddr data))
         (multiple-value-bind (argument final)
             (read rest)
           (values (list function argument)
                   final))))
      ((eql 1 first)
       (let ((ones (loop for next = (rest data)
                           then (rest next)
                         until (eql 0 (first next))
                         collect 1)))
         (values
          (length ones)
          (nthcdr (1+ (length ones))
                  (rest data))))))))

;; TODO: Iterative version and hash-table-based env?
(defun %eval (term &optional env)
  "Recursive/procedural CEK machine reference implementation (Wikipedia.)"
  (typecase term
    (integer (values (elt env term) env))
    (cons
     (cond
       ((lambda-p term)
        (values term env))
       (t
        (multiple-value-bind (fn new-env)
            (%eval (first term) env)
          (apply fn (%eval (second term) env) new-env)))))))

(defun apply (fn arg env)
  (%eval (second fn) (cons arg env)))

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

(defun eval (term)
  (multiple-value-bind (term env)
      (%eval term)
    (tree-transform-if
     (lambda (x d)
       (closed-p x))
     (lambda (x d)
       (%eval x))
     (plug-env term env))))

(optimize '((Λ (Λ (1 (1 0)))) (Λ 0)))

(defgeneric optimize (term)
  (:documentation "Optimize the TERM to be shorter and more correct."))

(defgeneric compile (expr &optional stack)
  (:documentation "Compile Lispy EXPR into binary lambdas.

Some Lisp values are automatically converted to their LC versions:
Integers: (compile '(1)) => (λ (λ (1 0)))
Booleans: (compile '(t)) => (λ (λ 1))
Characters: (compile '(#\Tab)) => (λ (λ (1 (1 (1 (1 (1 (1 (1 (1 (1 0)))))))))))
Strings: (compile '(\"hi\")) => You have to believe me on this one
Quoted lists of the above: (compile '('(t))) => (λ ((0 (λ (λ 1))) (λ (λ 0))))

All the compilation is reversible with `coerce', given the right type.")
  (:method ((expr list) &optional stack)
    (cond
      ((eq 'quote (first expr))
       (labels ((compile-list (list)
                  (if list
                      `(λ ((0 ,(compile (first list)))
                           ,(compile-list (rest list))))
                      '(λ (λ 0)))))
         (compile-list (second expr))))
      ((and (eq 'lambda (first expr))
            (second expr))
       (list 'λ
             (compile (list 'lambda (rest (second expr))
                            (third expr))
                      (cons (first (second expr))
                            stack))))
      ((eq 'lambda (first expr))
       (compile (third expr) stack))
      (t
       (reduce #'list expr
               :key #'(lambda (e)
                        (compile e stack))))))
  (:method ((expr symbol) &optional stack)
    (position expr stack))
  (:method ((expr integer) &optional stack)
    (declare (ignorable stack))
    `(λ (λ ,(loop for i below expr
                  for num = (list 1 0)
                    then (list 1 num)
                  finally (return num)))))
  (:method ((expr character) &optional stack)
    (declare (ignorable stack))
    (compile (char-code expr)))
  (:method ((expr (eql t)) &optional stack)
    (or (position expr stack)
        '(λ (λ 1))))
  (:method ((expr null) &optional stack)
    (or (position expr stack)
        '(λ (λ 0))))
  (:method ((expr string) &optional stack)
    (declare (ignorable stack))
    (labels ((convert (rest)
               (if rest
                   `(λ ((0 ,(compile (first rest)))
                        ,(convert (rest rest))))
                   (compile nil))))
      (convert (cl:coerce expr 'list)))))

(defgeneric coerce (term type &optional inner-type final-type)
  (:documentation "Try converting the TERM to a Lisp TYPE.
In case of success, return the respective TYPEd value.
In case of failure, return the TERM unaltered.
TYPE might be one of:
- BOOLEAN: T/NIL.
- INTEGER/NUMBER: Churn numeral -> integer.
- CHARACTER: Standard character (uses NUMBER method).
- CONS/LIST: Convert to list of INNER-TYPE with last cdr of FINAL-TYPE.
- STRING: A valid string (uses LIST off CHARACTERs method).")
  (:method ((term list) (type null) &optional inner-type final-type)
    (declare (ignorable inner-type final-type))
    (coerce term t)))

(defmethod coerce ((term list) (type (eql 'boolean)) &optional inner-type final-type)
  (declare (ignorable inner-type final-type))
  (cond
    ((equal '(λ (λ 1)) term)
     t)
    ((equal '(λ (λ 0)) term)
     nil)
    (t term)))

(defmethod coerce ((term list) (type (eql 'integer)) &optional inner-type final-type)
  (declare (ignorable inner-type final-type))
  (or (ignore-errors
       (loop with tail = (cadadr term)
             while (and (eql 1 (first tail))
                        (listp (second tail)))
             count 1 into num
             do (setf tail (second tail))
             finally (return (if (and (listp tail)
                                      (eql 1 (first tail))
                                      (eql 0 (second tail)))
                                 (1+ num)
                                 nil))))
      term))

(defmethod coerce ((term list) (type (eql 'number)) &optional inner-type final-type)
  (declare (ignorable inner-type final-type))
  (coerce term 'integer))

(defmethod coerce ((term list) (type (eql 'character)) &optional inner-type final-type)
  (declare (ignorable inner-type final-type))
  (code-char (coerce term 'integer)))

(defmethod coerce ((term list) (type (eql 'cons)) &optional inner-type final-type)
  ;; Manage full term (with lambda in the front) instead?
  (or (ignore-errors
       (loop with cons = term
             while (and (listp (cadr cons))
                        (listp (caadr cons))
                        (eql 0 (caaadr cons)))
             collect (if inner-type
                         (coerce (car (cdaadr cons)) inner-type)
                         (car (cdaadr cons)))
               into elems
             do (setf cons (cadadr cons))
             finally (if (equal '(λ (λ 0)) cons)
                         (return elems)
                         (progn
                           (setf (cdr (last elems))
                                 (cond
                                   (final-type (coerce cons final-type))
                                   (inner-type (coerce cons inner-type))
                                   (t cons)))
                           (return elems)))))
      term))

(defmethod coerce ((term list) (type (eql 'list)) &optional inner-type final-type)
  (coerce term 'cons inner-type final-type))

(defmethod coerce ((term list) (type (eql 'string)) &optional inner-type final-type)
  (declare (ignorable inner-type final-type))
  (cl:coerce (coerce term 'cons 'character)
             'string))

(defmethod coerce ((term list) (type (eql t)) &optional inner-type final-type)
  (declare (ignorable inner-type final-type))
  (flet ((try-convert (term type)
           (let ((converted (coerce term type)))
             (if (eq converted term)
                 nil
                 converted))))
    (or (try-convert term 'boolean)
        (try-convert term 'integer)
        (let* ((cons-convert (try-convert term 'cons)))
          (when cons-convert
            (loop for elem in cons-convert
                  for bool = (coerce elem 'boolean)
                  for int = (coerce elem 'integer)
                  when (not (equal bool elem))
                    collect bool
                  else when (not (equal int elem))
                         collect int
                  else
                    collect elem)))
        term)))

(defun term->bit-list (term)
  (cond
    ((integerp term)
     (append (loop for i from term downto 0
                   collect 1)
             (list 0)))
    ((lambda-p term)
     (append (list 0 0) (term->bit-list (second term))))
    ((listp term)
     (append (list 0 1)
             (term->bit-list (first term))
             (term->bit-list (second term))))))

(defgeneric write (term &key stream pretty binary literal)
  (:documentation "Write the `read' or `compile'd TERM to STREAM.
If PRETTY, try to convert TERM to number, list, or string.
When BINARY, write raw bytes instead of one & zero chars.
When literal, print the IR for the TERM.
In the absence of the above, print ones and zeros for TERM.")
  (:method ((term list) &key (stream t) (literal nil) (pretty nil) (binary nil))
    (let ((initial-stream stream)
          (stream (etypecase stream
                    ((eql t) *standard-output*)
                    (null (make-string-output-stream
                           :element-type (if binary
                                             '(unsigned-byte 8)
                                             'character)))
                    (stream stream))))
      (cond
        (binary
         (loop for (one two three four five six seven eight . rest)
                 on (term->bit-list term) by (lambda (bits)
                                               (nthcdr 8 bits))
               while one
               do (write-byte (+ (ash one 8)
                                 (ash (or two 0) 7)
                                 (ash (or three 0) 6)
                                 (ash (or four 0) 5)
                                 (ash (or five 0) 4)
                                 (ash (or six 0) 3)
                                 (ash (or seven 0) 2)
                                 (or eight 0))
                              stream)))
        (pretty
         (cl:write (coerce term t) :stream stream
                                   :escape t :circle nil :readably t
                                   :level nil :length nil :right-margin nil))
        (literal
         (cl:write term :stream stream
                        :escape t :circle nil :readably t
                        :level nil :length nil :right-margin nil))
        (t (loop for bit in (term->bit-list term)
                 do (format stream "~d" bit))))
      (if (null initial-stream)
          (get-output-stream-string stream)
          (values)))))

(defgeneric print (term &optional (stream t))
  (:documentation "Print the literal (like (Λ (Λ 1))) BLC representation of the TERM to STREAM.")
  (:method ((term list) stream)
    (write term :stream stream :literal t)))

(defgeneric princ (term &optional (stream t))
  (:documentation "Print the prettified (converted to Lisp whenever possible) TERM to STREAM.")
  (:method ((term list) stream)
    (write term :stream stream :pretty t)))
