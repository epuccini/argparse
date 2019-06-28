;;; -----------------------------------------------------
;;; main
;;; -----------------------------------------------------
;;; @File:     argparse/src/main.lisp
;;; @Date:     09:18:24 of Tuesday, 6/18/2019 (GMT+1)
;;; @Author:   Edward Puccini
;;; -----------------------------------------------------

(use-package :argparse)

(require 'argparse)
(require 'asdf)
(require 'clod)

#+(or cmu sbcl)
(setf *load-verbose* nil
      *load-print* nil
      asdf:*asdf-verbose* nil
      *compile-verbose* nil
      *compile-print* nil)

(defun main ()
  ;; init-parser and parse
  ;; set custom help message by setting *help-message*
  (let ((argument-data
         (with-arguments-hash-table
           "argparse"
           "An argument parser for commandline applications."
           "v1.0.4.0"
           '(:argument "--list"
             :description "List flag"
             :group "Listing"
             :type 'flag)
           '(:argument "--input"
             :description "Input file"
             :group "Convert"
             :type 'string)
           '(:argument "--output"
             :description "Output file"
             :group "Convert"
             :type 'string)
           '(:argument "--username"
             :description "User login name"
             :group "Convert"
             :type 'string)
           '(:argument "--endpoint"
             :description "Endpoint"
             :group "Convert"
             :type 'string))))
    (handle-unknown-arguments argument-data) ;; if you want to print unknown args
    (handle-missing-arguments argument-data) ;; if you want to restrict args to groups
    ;; print values
    (terpri)
    (format t "--list ~a~%" (get-argument-value argument-data "--list"))
    (format t "--input ~a~%" (get-argument-value argument-data "--input"))
    (format t "--output ~a~%" (get-argument-value argument-data "--output"))
    (format t "--username ~a~%" (get-argument-value argument-data "--username"))
    (format t "--endpoint ~-a~%" (get-argument-value argument-data "--endpoint"))
    (format t "--help ~a~%" (get-argument-value argument-data "--help"))
    ;; not existent value
    (format t "--test ~a~%" (get-argument-value argument-data "--test"))))
  
(defun build ()
  "Save executable with necessary options."
  (save-lisp-and-die "argparse.exe" :executable t :toplevel 'main :save-runtime-options t))

