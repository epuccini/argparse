;;; -----------------------------------------------------
;;; argparse
;;; -----------------------------------------------------
;;; File:     argparse/src/argparse.lisp
;;; Date:     09:18:24 of Tuesday, 6/18/2019 (GMT+1)
;;; Author:   Edward Puccini
;;; -----------------------------------------------------
(defvar *arguments* '())
(defvar *progname* nil)
(defvar *progdesc* nil)

(defun command-line-args ()
  (or
   #+ECL si:*command-args*
   #+SBCL sb-ext:*posix-argv*
   #+GCL si::*command-args*
   #+LISPWORKS system:*line-arguments-list*
   #+CMU extensions:*command-line-words*
   #+CLISP si:*command-args*
   nil))

(defun add-progname (name desc)
  (setf *arguments* nil)
  (setf *progname* name)
  (setf *progdesc* desc))

(defun add-argument-flag(arg)
  (push (list arg "") *arguments*))

(defun add-argument(arg)
  (push (list arg (concatenate 'string "[" (subseq arg 2 (length arg)) "]")) *arguments*))

(defun print-help()
  (format t "~a ~{~{~a ~}~}" *progname* *arguments*)
  (terpri)(terpri)
  (princ *progdesc*)
  (terpri)(terpri))
  
