 <h1>argparse - simple argument parser library for common lisp</h1><br><br>
 
 Dependencies are "alexandria" and "cl-ppcre". <br><br>
 To load and use this library, cd into the "src/" directory, start your lisp (testet on sbcl, ecl, ccl)
 and execute:
 
	(asdf:load-system :argparse)
   
To start arparse-example application:<br><br>

	(asdf:load-system :argparse-app)
  	(main)
  
This is the application example:

      (defun main ()
        ;; init-parser and parse
        ;; set custom help message by setting *help-message*
        (let ((argument-data
               (argparse:with-arguments-hash-table
                 "argparse"
                 "An argument parser for commandline applications."
                 "v1.0.4.0"
                 '(:argument "--list"
                   :description "List flag"
                   :group "Listing"
                   :type 'flag)
                 '(:argument "--input"
                   :description "Input file"
                   :group "Convert"
                   :type 'string)
                 '(:argument "--output"
                   :description "Output file"
                   :group "Convert"
                   :type 'string)
                 '(:argument "--username"
                   :description "User login name"
                   :group "Convert"
                   :type 'string)
                 '(:argument "--endpoint"
                   :description "Endpoint"
                   :group "Convert"
                   :type 'string))))
          (argparse:handle-unknown-arguments argument-data) ;; if you want to print unknown args
          (argparse:handle-missing-arguments argument-data) ;; if you want to restrict args to groups
          ;; print values
          (terpri)
          (format t "--list ~a~%" (argparse:get-argument-value argument-data "--list"))
          (format t "--input ~a~%" (argparse:get-argument-value argument-data "--input"))
          (format t "--output ~a~%" (argparse:get-argument-value argument-data "--output"))
          (format t "--username ~a~%" (argparse:get-argument-value argument-data "--username"))
          (format t "--endpoint ~-a~%" (argparse:get-argument-value argument-data "--endpoint"))
          (format t "--help ~a~%" (argparse:get-argument-value argument-data "--help"))
          ;; not existent value
          (format t "--test ~a~%" (argparse:get-argument-value argument-data "--test"))))
