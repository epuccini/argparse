;;; -----------------------------------------------------
;;; argparse
;;; -----------------------------------------------------
;;; File:     argparse/src/argparse.lisp
;;; Date:     09:18:24 of Tuesday, 6/18/2019 (GMT+1)
;;; Author:   Edward Puccini
;;; -----------------------------------------------------
(require 'cl-ppcre)

(defvar *arguments* '())
(defvar *argument-values* '())
(defvar *progname* nil)
(defvar *progdesc* nil)

(defun command-line-args ()
  "Get command line arguments."
  (or
   #+ECL si:*command-args*
   #+SBCL #("--input" "input.txt" "--output" "output.txt" "--username" "epuccini" "--endpoint" "localhost" "--help") ;; sb-ext:*posix-argv*
   #+GCL si::*command-args*
   #+LISPWORKS system:*line-arguments-list*
   #+CMU extensions:*command-line-words*
   #+CLISP si:*command-args*
   nil))

(defun add-progname (name desc)
  "Add program name and description."
  (setf *arguments* nil)
  (setf *argument-values* (make-hash-table))
  (setf *progname* name)
  (setf *progdesc* desc))

(defun add-argument-flag (arg)
  "Add argument flag."
  (push (list arg "") *arguments*))

(defun add-argument (arg)
  "Add argument with one value."
  (push (list arg (concatenate 'string "[" (subseq arg 2 (length arg)) "]")) *arguments*))

(defun print-help ()
  "Print help text if set."
  (format t "~a ~{~{~a ~}~}" *progname* *arguments*)
  (terpri)(terpri)
  (princ *progdesc*)
  (terpri)(terpri))

(defun find-arg (arg argv)
  "Find argument and return parameter at once."
  (let ((result nil)
        (flag nil))
    ;; check arg list
    (reduce #'(lambda (a b)
                (progn
                  (if (numberp (search arg a))
                      (progn
                        (setf flag t)
                        (setf result b)
                        b))
                  (if (numberp (search arg b))
                      (progn
                        (setf flag t)
                        (setf result t)
                        t))
                  b)) argv)
    (values flag result)))

(defun parse-arguments ()
  "Parse all given arguments in command-line."
  (let ((cmd-array (command-line-args)))
    (mapcar #'(lambda (arg)
                (destructuring-bind (a v) arg
                  (multiple-value-bind (f r) (find-arg a cmd-array)
                    (if (equal v "")
                        (setf (gethash a *argument-values*) f)
                        (setf (gethash a *argument-values*) r)))))
            *arguments*)))


(defun get-argument (hsh)
  "Get hash argument value."
  (gethash hsh *argument-values*))
