;;; -----------------------------------------------------
;;; main
;;; -----------------------------------------------------
;;; File:     argparse/src/main.lisp
;;; Date:     09:18:24 of Tuesday, 6/18/2019 (GMT+1)
;;; Author:   Edward Puccini
;;; -----------------------------------------------------

(require 'argparse)

(defun main ()
  ;; init-parser and parse
  (setup-argument-parser "argparse" "A request-response-tool")
  (add-argument-flag "--help" "Display help text flag")
  (add-argument "--input" "Input file")
  (add-argument "--output" "Output file")
  (add-argument "--username" "User login name")
  (add-argument "--endpoint" "RTC endpoint")
  (parse-arguments)
  (if (or (= (number-of-args) 1) (get-argument-value "--help"))
      (progn
        (print-help)
        (print-unknown-arguments)
        (exit)))
  (print-unknown-arguments)
  ;; print values
  (terpri)
  (format t "--input ~a~%" (get-argument-value "--input"))
  (format t "--output ~a~%" (get-argument-value "--output"))
  (format t "--username ~a~%" (get-argument-value "--username"))
  (format t "--endpoint ~-a~%" (get-argument-value "--endpoint"))
  (format t "--help ~a~%" (get-argument-value "--help"))
  ;; not existent value
  (format t "--test ~a~%" (get-argument-value "--test")))
        ;(exit))))
  
