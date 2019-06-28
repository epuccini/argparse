;;; -----------------------------------------------------
;;; argparse-app
;;; -----------------------------------------------------
;;; File:     argparse/src/argparse-app.asd
;;; Date:     09:18:24 of Tuesday, 6/18/2019 (GMT+1)
;;; Author:   Edward Puccini
;;; -----------------------------------------------------

(require 'asdf)

(defsystem "argparse-app"
    :description "An argument parser for commandline evaluation. Example application."
    :version "1.0.4.0"
    :maintainer "Edward Puccini"
    :licence "BSD"
    :description "Generated by pm 2017."
    :depends-on ("cl-ppcre" "alexandria")
    :components ((:file "package")
                 (:file "argparse")
                 (:file "main")))
