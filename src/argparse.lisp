;;; -----------------------------------------------------
;;; argparse
;;; -----------------------------------------------------
;;; File:     argparse/src/argparse.lisp
;;; Date:     09:18:24 of Tuesday, 6/18/2019 (GMT+1)
;;; Author:   Edward Puccini
;;; -----------------------------------------------------
(require 'cl-ppcre)
(require 'asdf)

#+(or cmu sbcl)
(setf *load-verbose* nil
      *load-print* nil
      asdf:*asdf-verbose* nil
      *compile-verbose* nil
      *compile-print* nil)

(defvar *arguments* '())
(defvar *argument-description* '())
(defvar *argument-values* '())
(defvar *progname* nil)
(defvar *progdesc* nil)

(defun command-line-args ()
  "Get command line arguments."
  (or
   #+ECL si:*command-args*
   ;#+SBCL #("argparse.exe" "--input" "input.txt" "--output" "output.txt" "--username" "epuccini" "--endpoint" "localhost" "--help" "--wrong")
   #+SBCL sb-ext:*posix-argv*
   #+GCL si::*command-args*
   #+LISPWORKS system:*line-arguments-list*
   #+CMU extensions:*command-line-words*
   #+CLISP si:*command-args*
   nil))

(defun setup-argument-parser (name desc)
  "Clear arrays and create new hashtable. Add program name and description."
  (setf *arguments* nil)
  (setf *argument-description* nil)
  (setf *argument-values* (make-hash-table))
  (setf *progname* name)
  (setf *progdesc* desc))

(defun add-argument-flag (arg desc)
  "Add argument flag."
    (push (list arg "") *arguments*)
    (push (list arg desc) *argument-description*))

(defun add-argument (arg desc)
  "Add argument with one value."
    (push (list arg
                (concatenate 'string "[" (subseq arg 2 (length arg)) "]"))
          *arguments*)
    (push (list arg desc) *argument-description*))

(defun print-help ()
  "Print help text if set."
  (format t "~a ~{~{~a ~}~}" *progname* (reverse *arguments*))
  (terpri)(terpri)
  (princ *progdesc*)
  (terpri)(terpri)
  (format t "~{~{   ~1,4T~A~2,8T~A~%~}~}" (reverse *argument-description*)))


(defun find-arg (arg argv)
  "Find argument and return parameter at once."
  (let ((result nil)
        (flag nil)
        (short-arg (subseq arg 1 3)))
    ;; check arg list
    (reduce #'(lambda (a b)
                (progn
                  (if (or
                       (numberp (search arg a))
                       (numberp (search short-arg a)))
                      (progn
                        (setf flag t)
                        (setf result b)
                        b))
                  (if (or
                       (numberp (search arg b))
                       (numberp (search short-arg b)))
                       (progn
                        (setf flag t)
                        (setf result t)
                        t))
                  b)) argv)
    (values flag result)))

(defun parse-arguments ()
  "Parse all given arguments in command-line."
  (let ((cmd-array (command-line-args)))
    (if (= (length cmd-array) 1)
        (progn
          (print-help)
          (exit)))
    (mapcar #'(lambda (arg)
                (destructuring-bind (a v) arg
                  (multiple-value-bind (f r) (find-arg a cmd-array)
                    (if (equal v "")
                        (setf (gethash a *argument-values*) f)
                        (setf (gethash a *argument-values*) r)))))
            (reverse *arguments*))))


(defun print-unknown-arguments ()
  "Print unknown or wrong arguments in commandline."
  (let ((cmd-arg (map 'list #'identity (command-line-args))))
    ;; remove progname - with .exe on windows
    #+Windows
    (setf cmd-arg
          (remove-if #'(lambda (val)
                         (equal val (concatenate 'string *progname* ".exe"))) cmd-arg))
    (setf cmd-arg (remove-if #'(lambda (val) (equal val *progname*)) cmd-arg))
    ;; remove existing args - the left ones are unknown
    (mapcar #'(lambda (arg)
                (destructuring-bind (arg v) arg
                  (let ((short-arg (subseq arg 1 3)))
                    (setf cmd-arg (remove-if #'(lambda (val) (equal val arg)) cmd-arg))
                    (setf cmd-arg (remove-if #'(lambda (val) (equal val short-arg)) cmd-arg))
                    (setf cmd-arg (remove-if #'(lambda (val) (equal val (get-argument arg))) cmd-arg)))))
            (reverse *arguments*))
    (if (> (length cmd-arg) 0)
        (progn
          (format t "~a argument(s) left~%" (length cmd-arg))
          (format t "Wrong or unknown arguments: ~{~a ~}~%" cmd-arg)
          (terpri)
          (exit)))))


(defun get-argument (hsh)
  "Get hash argument value."
  (gethash hsh *argument-values*))
