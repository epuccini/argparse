;;; -----------------------------------------------------
;;; Demo preset
;;; -----------------------------------------------------
;;; File:     C:/Users/epuccini/Documents/Code/common-lisp/projects/argparse/src/argparse-test.asd
;;; Date:     09:18:24 of Tuesday, 6/18/2019 (GMT+1)
;;; Author:   Edward Puccini
;;; -----------------------------------------------------

(require 'asdf)

(defsystem "argparse-test"
    :version "0.0.0"
    :maintainer "Edward Puccini"
    :licence "BSD"
    :description "Generated by pm 2017."
    :components ((:file "package")
                 (:file "argparse")
                 (:file "test")))