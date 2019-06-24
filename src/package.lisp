;;; -----------------------------------------------------
;;; package
;;; -----------------------------------------------------
;;; File:     argparse/src/package.lisp
;;; Date:     09:18:24 of Tuesday, 6/18/2019 (GMT+1)
;;; Author:   Edward Puccini
;;; -----------------------------------------------------

(defpackage :argparse
  (:use "COMMON-LISP")
  (:export
   #:command-line-args
   #:setup-argument-parser
   #:add-argument-flag
   #:add-argument
   #:print-help
   #:parse-arguments
   #:get-argument-value
   #:print-unknown-arguments
))

