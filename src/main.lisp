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
  (setup-argument-parser "argparse"
                         "An argument parser for commandline applications."
                         "v1.0.2.1")
  (add-argument-flag "--list" "List flag" "Listing")
  (add-argument "--input" "Input file" "Input")
  (add-argument "--output" "Output file" "Output")
  (add-argument "--username" "User login name" "Output")
  (add-argument "--endpoint" "Endpoint" "Output")
  (parse-arguments)
  (handle-unknown-arguments) ;; call this function if you want to print unknown args
  (handle-missing-arguments) ;; call this function if you want to restrict args to groups
  ;; print values
  (terpri)
  (format t "--list ~a~%" (get-argument-value "--list"))
  (format t "--input ~a~%" (get-argument-value "--input"))
  (format t "--output ~a~%" (get-argument-value "--output"))
  (format t "--username ~a~%" (get-argument-value "--username"))
  (format t "--endpoint ~-a~%" (get-argument-value "--endpoint"))
  (format t "--help ~a~%" (get-argument-value "--help"))
  ;; not existent value
  (format t "--test ~a~%" (get-argument-value "--test")))
  
