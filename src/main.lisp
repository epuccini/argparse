;;; -----------------------------------------------------
;;; main
;;; -----------------------------------------------------
;;; File:     argparse/src/main.lisp
;;; Date:     09:18:24 of Tuesday, 6/18/2019 (GMT+1)
;;; Author:   Edward Puccini
;;; -----------------------------------------------------

(require 'argparse)
(require 'asdf)

#+(or cmu sbcl)
(setf *load-verbose* nil
      *load-print* nil
      asdf:*asdf-verbose* nil
      *compile-verbose* nil
      *compile-print* nil)

(defun main ()
  ;; init-parser and parse
  ;; set custom help message by setting *help-message*
  (let ((param
         (argparse:with-arguments-hash-table
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
    (argparse:handle-unknown-arguments param) ;; if you want to print unknown args
    (argparse:handle-missing-arguments param) ;; if you want to restrict args to groups
    ;; print values
    (terpri)
    (format t "--list ~a~%" (argparse:get-argument-value param "--list"))
    (format t "--input ~a~%" (argparse:get-argument-value param "--input"))
    (format t "--output ~a~%" (argparse:get-argument-value param "--output"))
    (format t "--username ~a~%" (argparse:get-argument-value param "--username"))
    (format t "--endpoint ~-a~%" (argparse:get-argument-value param "--endpoint"))
    (format t "--help ~a~%" (argparse:get-argument-value param "--help"))
    ;; not existent value
    (format t "--test ~a~%" (argparse:get-argument-value param "--test"))))
  
(defun build ()
  "Save executable with necessary options."
  (save-lisp-and-die "argparse" :executable t :toplevel 'main :save-runtime-options t))

