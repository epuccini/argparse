;;; -----------------------------------------------------
;;; package
;;; -----------------------------------------------------
;;; File:     argparse/src/package.lisp
;;; Date:     09:18:24 of Tuesday, 6/18/2019 (GMT+1)
;;; Author:   Edward Puccini
;;; -----------------------------------------------------

(defpackage :argparse
  (:documentation "An argument parser for commandline evaluation.")
  (:use #:common-lisp)
  (:shadow
   #:get-argument-value
   #:handle-unknown-arguments
   #:handle-missing-arguments)
  (:export
   #:command-line-args
   #:with-arguments-hash-table
   #:setup-argument-parser
   #:add-argument-flag
   #:add-argument
   #:print-help
   #:parse-arguments
   #:get-argument-value
   #:handle-unknown-arguments
   #:handle-missing-arguments
   #:build)
  (:intern
   #:identify-group
   #:find-argument))

