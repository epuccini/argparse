;;; -----------------------------------------------------
;;; argparse - Argument parser
;;; -----------------------------------------------------
;;; File:     argparse/src/argparse.lisp
;;; Date:     09:18:24 of Tuesday, 6/18/2019 (GMT+1)
;;; Author:   Edward Puccini
;;; -----------------------------------------------------

(in-package :argparse)

(require 'alexandria)

(defvar *test* nil "Test variable")

(defun command-line-args ()
  "Get command line arguments.
*Returns
- Array with command-line arguments with executable as first element."
  (or
   #+ECL si:*command-args*
   #+SBCL sb-ext:*posix-argv*
   #+GCL si::*command-args*
   #+LISPWORKS system:*line-arguments-list*
   #+CMU extensions:*command-line-words*
   #+CLISP si:*command-args*
   nil))

(defun setup-argument-parser (&key (name "") (description "") ( version ""))
  "Clear arrays and create new hashtable. Add program name and description. 
Add --help and --version argument.
*Arguments
- NAME :: Program executable name (without extension).
- DESCRIPTION :: Program description.
- VERSION :: Program executable version-number.
*Returns
- created Program data hashtable."
  (let ((argument-data (make-hash-table)))
    (setf (gethash "Programname" argument-data) name)
    (setf (gethash "Programdescription" argument-data) description)
    (setf (gethash "Programversion" argument-data) version)
    (setf argument-data
          (add-argument argument-data
                        :argument "--help"
                        :description "Shows this help message and exit"
                        :group "Verbose"
                        :type 'flag))
    (setf argument-data
          (add-argument argument-data
                        :argument "--version"
                        :description "Show program version and exit"
                        :group "Application"
                        :type 'flag))
    argument-data))

(defun add-argument (argument-data &key (argument "") (description "") (group "") (type ""))
  "Add argument with one value. Group to combine arguments 
which should all have to be set at once.
*Arguments
- ARGUMENT :: Unix like option full name string. Shortcut will be from --XXXXX to -X
- DESCRITPTION :: Option description string.
- GROUP :: Group name string
- TYPE :: Types: 'FLAG and 'STRING
*Returns
- modified Program data hashtable"
  (if (equal type 'flag)
      (push (list argument "" description "") (gethash group argument-data))
      (push (list argument
                  (concatenate 'string
                               "[" (subseq argument 2 (length argument)) "]") description "")
            (gethash group argument-data)))
  argument-data)

(defun print-help (argument-data)
  "Print help text if set. Otherwise auto-generated help text.
*Arguments
ARGUMENT-DATA :: Program data hashtable"
      (let ((keys (get-group-keys argument-data)))
        ;; print usage line
        (format t "Usage: ~a " (gethash "Programname" argument-data))
        (loop for group in keys do
             (loop for quadruple in (gethash group argument-data) do
                  (destructuring-bind (arg field desc value) quadruple
                    (declare (ignore desc value))
                    (if (> (length field) 0)
                        (format t "[~a ~a] " arg field)
                        (format t "[~a] " arg)))))
        ;; print group of arguments and descriptions
        (format t "~%~%~a~%" (gethash "Programdescription" argument-data))
        (loop for key in keys do
             (format t "~%~a:~%" key)
             (loop for quadruple in (reverse (gethash key argument-data)) do
                  (destructuring-bind (arg field desc value) quadruple
                    (declare (ignore field value))
                    (format t "~1,4T~a, ~A ~3,8T~A~%" (subseq arg 1 3) arg desc))))))

(defun print-version (argument-data)
  "Print version string.
*Arguments
ARGUMENT-DATA :: Program data hashtable"
  (format t "~a: ~a~%"
          (gethash "Programname" argument-data)
          (gethash "Programversion" argument-data)))

(defun find-argument (arg argv)
  "Find argument and return parameter at once.
*arguments
- ARG :: Argument-string --XXXXXX
- ARGV :: Command-line argument array
*Returns
- T for flags, value for options"
  (let ((result nil)
        (flag nil)
        (short-arg (subseq arg 1 3)))
    ;; check arg list
    (reduce #'(lambda (a b)
                (progn
                  (if (or
                       (numberp (search arg a))
                       (numberp (search short-arg a)))
                      (progn
                        (setf flag t)
                        (setf result b)
                        b))
                  (if (or
                       (numberp (search arg b))
                       (numberp (search short-arg b)))
                       (progn
                        (setf flag t)
                        (setf result t)
                        t))
                  b)) argv)
    (values flag result)))

(defun get-group-keys (argument-data)
  "Get all group keys from program data hashtable.
*Arguments
- ARGUMENT-DATA :: Program data hashtable.
*Returns
- Group key array"
  (let ((keys (reverse (remove-if #'(lambda (key)
                                   (equal (subseq key 0 7) "Program"))
                               (alexandria:hash-table-keys argument-data)))))
    keys))

(defun parse-arguments (argument-data)
  "Parse all given arguments in command-line.
*Arguments
- ARGUMENT-DATA :: Program data hashtable.
*Returns
- modified Program data hashtable"
  (handler-case
      (let ((cmd-array (command-line-args))
            (keys (get-group-keys argument-data)))
        ;; parse
        (loop for group in keys do
             (mapcar #'(lambda (lst)
                         (progn
                           (setf (gethash group argument-data)
                                 (remove-if #'(lambda (a) (equal a lst))
                                            (gethash group argument-data)))
                           (destructuring-bind (a f d v) lst
                             (declare (ignore d v))
                             (multiple-value-bind (flag value) (find-argument a cmd-array)
                               (if (equal f "")
                                   (setf (nth 3 lst) flag)
                                   (setf (nth 3 lst) value))
                               (push lst (gethash group argument-data))))))
             (gethash group argument-data)))
        (if (get-argument-value argument-data "--help")
            (progn
              (print-help argument-data)
              (sb-ext:exit)))
        (if (get-argument-value argument-data "--version")
            (progn
              (print-version argument-data)
              (sb-ext:exit)))
        argument-data)
  (error (condition)
         (format t "Error while parsing arguments, condition ~a~%" condition))))

(defun handle-unknown-arguments (argument-data)
  "Print unknown or wrong arguments in commandline. Exit on error.
*Arguments
- ARGUMENT-DATA :: Program data hashtable."
  (let ((cmd-arg (map 'list #'identity (command-line-args)))
        (keys (get-group-keys argument-data)))
    ;; remove program-name - with .exe on windows
    #+Windows
    (setf cmd-arg
          (remove-if #'(lambda (val)
                         (equal val (concatenate 'string
                                                 (gethash "Programname" argument-data) ".exe"))) cmd-arg))
    (setf cmd-arg (remove-if #'(lambda (val)
                                 (equal val (gethash "Programname" argument-data))) cmd-arg))
    ;; remove existing args - the left ones are unknown
    (loop for group in keys do
         (mapcar #'(lambda (lst)
                     (destructuring-bind (arg f d v) lst
                       (declare (ignore f d v))
                       (let ((short-arg (subseq arg 1 3)))
                         (setf cmd-arg (remove-if
                                        #'(lambda (val)
                                            (equal val arg)) cmd-arg))
                         (setf cmd-arg (remove-if
                                        #'(lambda (val)
                                            (equal val short-arg)) cmd-arg))
                         (setf cmd-arg (remove-if
                                        #'(lambda (val)
                                            (equal val (get-argument-value argument-data arg))) cmd-arg)))))
                 (gethash group argument-data)))
    (if (> (length cmd-arg) 0)
        (progn
          (format t "~a argument(s) left~%" (length cmd-arg))
          (format t "Wrong or unknown arguments: ~{~a ~}~%" cmd-arg)
          (terpri)
          (sb-ext:exit)))))

(defun identify-group (argument-data arg)
  "Check which argument belongs to group.
*Arguments
- ARGUMENT-DATA :: Program data hashtable.
- ARG :: argument string
*Returns
- Group string"
  (loop for group in (get-group-keys argument-data) do
       (mapcar #'(lambda (lst)
                   (destructuring-bind (a f d v) lst
                     (declare (ignore f d v))
                     (if (or (equal a arg)
                             (equal (subseq a 1 3) arg))
                         (return group))))
               (gethash group argument-data))))
         
(defun handle-missing-arguments (argument-data)
  "Check for missing arguments. Print message and exit.
*Arguments
- ARGUMENT-DATA :: Program data hashtable."
  (let* ((first-arg (cadr (command-line-args)))
         (cmd-args (cdr (command-line-args)))
         (group (identify-group argument-data first-arg))
         (missing-args '())
         (group-args nil))
         (mapcar #'(lambda (lst)
                     (destructuring-bind (a f d v) lst
                       (declare (ignore f d))
                       (if a
                           (push a group-args))
                       (if  (typep v 'sequence) 
                           (push v group-args))))
                 (gethash group argument-data))
         (setf missing-args
               (remove-if #'(lambda (e)
                              (loop for arg in cmd-args do
                                   (if (or
                                        (equal e arg)
                                        (equal e (get-full-argument argument-data arg)))
                                       (return t))))
                          group-args))
    (if (> (length missing-args) 0)
        (progn
          (format t "Missing arguments: ~{~a ~}~%" (reverse missing-args))
          (sb-ext:exit)))))

(defun get-argument-value (argument-data arg)
  "Get hash argument value.
*Arguments
- ARGUMENT-DATA :: Program data hashtable.
- ARG :: Argument string
*Returns
- Value string"
  (let ((keys (get-group-keys argument-data)))
    ;; parse
    (loop for group in keys do
         (mapcar #'(lambda (lst)
                     (destructuring-bind (a f d v) lst
                       (declare (ignore f d))
                       (if (equal a arg)
                           (return v))))
                 (gethash group argument-data)))))


(defun get-full-argument (argument-data short-arg)
  "Get hash argument from shortcut arg.
*Arguments
- ARGUMENT-DATA :: Program data hashtable.
- SHORT-ARG :: Shortcut string of option/flag
*Returns
- Argument string"
  (let ((keys (get-group-keys argument-data)))
    ;; parse
    (loop for group in keys do
         (mapcar #'(lambda (lst)
                     (destructuring-bind (a f d v) lst
                       (declare (ignore f d v))
                       (if (equal (subseq a 1 3) short-arg)
                           (return a))))
                 (gethash group argument-data)))))

(defun number-of-args ()
  "Get number of arguments from commandline.
*Returns
- Length of command-line array int"
  (let ((cmd-array (command-line-args)))
    (length cmd-array)))

(defmacro with-arguments-hash-table (name description version &body arguments)
  "Compact argument-parser definition construct.
*Arguments
- NAME :: Application name string
- DESCRIPTION :: Application description string
- VERSION :: Application version string
- ARGUMENTS :: Option definition lists with parameter in the form of ADD-ARGUMENT
*Returns
- modified Program data hashtable"
  `(let ((argument-data
          (setup-argument-parser :name ,name :description ,description :version ,version)))
     ,@(loop for 'arg in arguments collect
            `(setf argument-data
                   (add-argument argument-data
                                  :argument (getf (quote ,arg) :argument)
                                  :description (getf (quote ,arg) :description)
                                  :group (getf (quote ,arg) :group)
                                  :type (getf (quote ,arg) :type))))
     (setf argument-data (parse-arguments argument-data))
     argument-data))
