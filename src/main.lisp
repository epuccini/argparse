;;; -----------------------------------------------------
;;; main
;;; -----------------------------------------------------
;;; File:     argparse/src/main.lisp
;;; Date:     09:18:24 of Tuesday, 6/18/2019 (GMT+1)
;;; Author:   Edward Puccini
;;; -----------------------------------------------------

(use-package 'argparse)

(defun main ()
  (add-progname "rtctool" "A request-response-tool")
  (add-argument-flag "--help")
  (add-argument "--input")
  (add-argument "--output")
  (add-argument "--username")
  (add-argument "--endpoint")
  (print-help)
  (exit))
  
