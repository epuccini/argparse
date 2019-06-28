;;; -----------------------------------------------------
;;; package
;;; -----------------------------------------------------
;;; File:     argparse/src/package.lisp
;;; Date:     09:18:24 of Tuesday, 6/18/2019 (GMT+1)
;;; Author:   Edward Puccini
;;; -----------------------------------------------------
(in-package :cl-user)

(defpackage :argparse
  (:documentation "An argument parser for commandline evaluation.")
  (:use #:cl #:alexandria)
  (:export
   #:with-arguments-hash-table
   #:command-line-args
   #:setup-argument-parser
   #:add-argument-flag
   #:add-argument
   #:print-help
   #:print-version
   #:parse-arguments
   #:get-argument-value
   #:find-argument
   #:get-group-keys
   #:identify-group
   #:number-of-args
   #:handle-unknown-arguments
   #:handle-missing-arguments
   #:build))

