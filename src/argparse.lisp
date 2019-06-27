;;; -----------------------------------------------------
;;; argparse - Argument parser
;;; -----------------------------------------------------
;;; File:     argparse/src/argparse.lisp
;;; Date:     09:18:24 of Tuesday, 6/18/2019 (GMT+1)
;;; Author:   Edward Puccini
;;; -----------------------------------------------------

(require 'cl-ppcre)
(require 'alexandria)

(defvar *help-message* nil)
(defvar *argument-data* (make-hash-table))
(defvar *program-name* nil)
(defvar *program-desc* nil)
(defvar *program-version* nil)

(defun command-line-args ()
  "Get command line arguments."
  (or
   #+ECL si:*command-args*
   #+SBCL sb-ext:*posix-argv*
   #+GCL si::*command-args*
   #+LISPWORKS system:*line-arguments-list*
   #+CMU extensions:*command-line-words*
   #+CLISP si:*command-args*
   nil))

(defun setup-argument-parser (name desc version)
  "Clear arrays and create new hashtable. Add program name and description. 
Add help argument."
  (setf *program-name* name)
  (setf *program-desc* desc)
  (setf *program-version* version)
  (add-argument "--help" "Shows this help message and exit" "Verbose" 'flag)
  (add-argument "--version" "Show program version and exit" "Application" 'flag))

(defun add-argument (arg desc group type)
  "Add argument with one value. Group to combine arguments 
which should all have to be set at once."
  (if (equal type 'flag)
      (push (list arg "" desc "") (gethash group *argument-data*))
      (push (list arg (concatenate 'string "[" (subseq arg 2 (length arg)) "]") desc "")
            (gethash group *argument-data*))))


(defun setup-argparse (name desc version &rest args)
  (setup-argument-parser name desc version)
  (loop for arg in args do
        (destructuring-bind (nm dc vl ty) arg
         (add-argument nm dc vl ty)))
  (parse-arguments))

(defmacro with-arguments (name desc version &rest args)
  `(progn
     (setup-argument-parser ,name ,desc ,version)
     ,@(loop for 'arg in args collect
            (destructuring-bind (ag dc gp tp) arg
               `(add-argument ,ag ,dc ,gp ,tp)))
     (parse-arguments)))

(defun print-help ()
  "Print help text if set. Otherwise auto-generated help text."
  (if *help-message*
      (format t "~a~%" *help-message*)
      (let ((keys (reverse
                   (alexandria:hash-table-keys *argument-data*))))
        ;; print usage line
        (format t "Usage: ~a " *program-name*)
        (loop for group in keys do
             (loop for quadruple in (gethash group *argument-data*) do
                  (destructuring-bind (arg field desc value) quadruple
                    (declare (ignore desc value))
                    (if (> (length field) 0)
                        (format t "[~a ~a] " arg field)
                        (format t "[~a] " arg)))))
        ;; print group of arguments and descriptions
        (format t "~%~%~a~%" *program-desc*)
        (loop for key in keys do
             (format t "~%~a:~%" key)
             (loop for quadruple in (reverse (gethash key *argument-data*)) do
                  (destructuring-bind (arg field desc value) quadruple
                    (declare (ignore field value))
                    (format t "~1,4T~a, ~A ~3,8T~A~%" (subseq arg 1 3) arg desc)))))))

(defun print-version ()
  "Print version string."
  (format t "~a: ~a~%" *program-name* *program-version*))

(defun find-arg (arg argv)
  "Find argument and return parameter at once."
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

(defun parse-arguments ()
  "Parse all given arguments in command-line."
  (handler-case
      (let ((cmd-array (command-line-args))
            (keys (alexandria:hash-table-keys *argument-data*)))
        ;; parse
        (loop for group in keys do
             (mapcar #'(lambda (lst)
                         (progn
                           (setf (gethash group *argument-data*)
                                 (remove-if #'(lambda (a) (equal a lst))
                                            (gethash group *argument-data*)))
                           (destructuring-bind (a f d v) lst
                             (declare (ignore d v))
                             (multiple-value-bind (flag value) (find-arg a cmd-array)
                               (if (equal f "")
                                   (setf (nth 3 lst) flag)
                                   (setf (nth 3 lst) value))
                               (push lst (gethash group *argument-data*))))))
             (gethash group *argument-data*)))
        (if (get-argument-value "--help")
            (progn
              (print-help)
              (exit)))
        (if (get-argument-value "--version")
            (progn
              (print-version)
              (exit))))
  (error (condition)
         (format t "Error while parsing arguments, condition ~a~%" condition))))

(defun handle-unknown-arguments ()
  "Print unknown or wrong arguments in commandline. Exit on error."
  (let ((cmd-arg (map 'list #'identity (command-line-args)))
        (keys (alexandria:hash-table-keys *argument-data*)))
    ;; remove program-name - with .exe on windows
    #+Windows
    (setf cmd-arg
          (remove-if #'(lambda (val)
                         (equal val (concatenate 'string *program-name* ".exe"))) cmd-arg))
    (setf cmd-arg (remove-if #'(lambda (val) (equal val *program-name*)) cmd-arg))
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
                                            (equal val (get-argument-value arg))) cmd-arg)))))
                 (gethash group *argument-data*)))
    (if (> (length cmd-arg) 0)
        (progn
          (format t "~a argument(s) left~%" (length cmd-arg))
          (format t "Wrong or unknown arguments: ~{~a ~}~%" cmd-arg)
          (terpri)
          (exit)))))

(defun identify-group (arg)
  "Check which argument belongs to group."
  (loop for group in (alexandria:hash-table-keys *argument-data*) do
       (mapcar #'(lambda (lst)
                   (destructuring-bind (a f d v) lst
                     (declare (ignore f d v))
                     (if (or (equal a arg)
                             (equal (subseq a 1 3) arg))
                         (return group))))
               (gethash group *argument-data*))))
         
(defun handle-missing-arguments ()
  "Check for missing arguments. Print message and exit."
  (let* ((first-arg (cadr (command-line-args)))
         (cmd-args (cdr (command-line-args)))
         (group (identify-group first-arg))
         (missing-args '())
         (group-args nil))
         (mapcar #'(lambda (lst)
                     (destructuring-bind (a f d v) lst
                       (declare (ignore f d))
                       (if a
                           (push a group-args))
                       (if  (typep v 'sequence) 
                           (push v group-args))))
                 (gethash group *argument-data*))
         (setf missing-args
               (remove-if #'(lambda (e)
                              (loop for arg in cmd-args do
                                   (if (or
                                        (equal e arg)
                                        (equal e (get-full-argument arg)))
                                       (return t))))
                          group-args))
    (if (> (length missing-args) 0)
        (progn
          (format t "Missing arguments: ~{~a ~}~%" (reverse missing-args))
          (exit)))))

(defun get-argument-value (arg)
  "Get hash argument value."
  (let ((keys (alexandria:hash-table-keys *argument-data*)))
    ;; parse
    (loop for group in keys do
         (mapcar #'(lambda (lst)
                     (destructuring-bind (a f d v) lst
                       (declare (ignore f d))
                       (if (equal a arg)
                           (return v))))
                 (gethash group *argument-data*)))))


(defun get-full-argument (short-arg)
  "Get hash argument from shortcut arg."
  (let ((keys (alexandria:hash-table-keys *argument-data*)))
    ;; parse
    (loop for group in keys do
         (mapcar #'(lambda (lst)
                     (destructuring-bind (a f d v) lst
                       (declare (ignore f d v))
                       (if (equal (subseq a 1 3) short-arg)
                           (return a))))
                 (gethash group *argument-data*)))))

(defun number-of-args ()
  "Get number of arguments from commandline."
  (let ((cmd-array (command-line-args)))
    (length cmd-array)))
