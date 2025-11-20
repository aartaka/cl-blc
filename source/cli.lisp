;;;; SPDX-FileCopyrightText: Artyom Bologov
;;;; SPDX-License-Identifier: BSD-2 Clause

(in-package :cl-blc)

(defun herefile (file)
  (uiop:merge-pathnames*
   (uiop:parse-native-namestring file)
   (uiop:getcwd)))

(defun in->stream (in)
  (cond
    ((and (stringp in)
          (string-equal "--" in))
     *standard-input*)
    ((and (stringp in)
          (uiop:file-exists-p (herefile in)))
     (open (herefile in)
           :direction :input
           :if-does-not-exist :create))
    ((streamp in)
     in)))

(defun out->stream (out)
  (cond
    ((and (stringp out)
          (string-equal "--" out))
     *standard-output*)
    ((stringp out)
     (open (herefile out)
           :direction :output
           :if-exists :supersede
           :if-does-not-exist :create))
    ((streamp out)
     out)))

(defun print-cased (res out type)
  (case type
    (:string (format out "~&~a~%" (coerce res 'string)))
    (:character (format out "~&~c~%" (coerce res 'character)))
    ((:number :integer) (format out "~&~d~%" (coerce res 'number)))
    (:boolean (format out "~&~:[false~;true~]~%" (coerce res 'boolean)))
    (:pretty (princ res out))))

(defun entry-point ()
  (let* ((args uiop:*command-line-arguments*)
         (command (first args))
         (help-p (or (null args)
                     (and (stringp args)
                          (member command '("help" "--help" "-help" "-h")
                                  :test #'string-equal)))))
    (when help-p
      (format t "CL-BLC is a toolkit for working with Binary Lambda Calculus.
Usage:
  blc [command] [args]

Commands are:
  help
  compile     prog type     bin
  run         prog type in  out
  uni2bin               in  out
  bin2uni               in  out
  lisp2bin              in  out
  lisp2uni              in  out

Compiled binary use:
  bin in out

PROG is a file with .Blc (true binary) or .blc (ASCII ones and zeros)
extension. The function contained within will be run with input
provided in IN, as a single cons list of characters.

TYPE, when compiling the PROG, or running it, is the output style:
- pretty: aggressively try to meaningfully print the returned
  value. As number, cons, boolean etc.
- string (default): print the (mandatory) returned cons list as
  string.
- number: print the resulting number.
- boolean: print the result as a boolean.

IN is either file name, Lisp expression (number, T/NIL boolean,
string, or list of thereof), or --, meaning standard input. When
reading from standard input, the input should be terminated with null
char (Ctrl-D on *nix).

OUT is either a file name or -- to print to standard output.~%")
      (uiop:shell-boolean-exit t))
    (case (intern (string-upcase command) :keyword)
      (:compile
       (destructuring-bind (prog &optional (type "string")
                              (bin (herefile (pathname-name (uiop:parse-native-namestring prog)))))
           (rest args)
         (let* ((prog (herefile prog))
                (term (read prog)))
           (setf uiop:*image-entry-point*
                 `(lambda ()
                    (destructuring-bind (&optional (in *standard-input*)
                                           (out *standard-output*))
                        uiop:*command-line-arguments*
                      (let ((res (eval (list (quote ,term)
                                             (compile (uiop:slurp-stream-string (in->stream in)))))))
                        (print-cased
                         res
                         (out->stream out)
                         ,(intern (string-upcase type) :keyword))))))
           (uiop:dump-image bin :executable t))))
      (:run
       (destructuring-bind (prog
                             &optional (type "string")
                             (in *standard-input*) (out *standard-output*))
           (rest args)
         (let* ((prog (herefile prog))
                (in (in->stream in))
                (out (out->stream out))
                (res (eval (list (read prog)
                                 (compile (uiop:slurp-stream-string in))))))
           (print-cased res out (intern (string-upcase type) :keyword))))
       (uiop:shell-boolean-exit t))
      (:uni2bin
       (destructuring-bind (in &optional (out *standard-output*))
           (rest args)
         (write (read in)
                :stream (out->stream out)
                :format :binary)))
      ;; TODO (:uni2lisp)
      (:bin2uni
       (destructuring-bind (in &optional (out *standard-output*))
           (rest args)
         (write (read in)
                :stream (out->stream out))))
      ;; TODO (:bin2lisp)
      (:lisp2uni
       (destructuring-bind (in &optional (out *standard-output*))
           (rest args)
         (write (compile (cl:read (in->stream in)))
                :stream (out->stream out))))
      (:lisp2bin
       (destructuring-bind (in &optional (out *standard-output*))
           (rest args)
         (write (compile (cl:read (in->stream in)))
                :stream (out->stream out)
                :format :binary))))))
