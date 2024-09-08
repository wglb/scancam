(in-package #:scancam)

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0)))

(defparameter *errors-encountered* 0)
(declaim (fixnum *errors-encountered*))

(defparameter *similar-images-deleted* 0)
(declaim (fixnum *similar-images-deleted*))

(defparameter *global-images-viewed* 0)
(declaim (fixnum *global-images-viewed*))


(define-flag *process*
  :default-value "scancam" 
  :selector "process"
  :type string
  :help "Which process"
  :documentation "Which sub-process to run")

(define-flag *time-gaps*
  :default-value 300
  :selector "time-gaps"
  :type integer
  :help "Calculate time gaps of images"
  :documentation "Gaps for find-time-gaps; remainder of command line arguments specify directories")

(define-flag *dbg-flag*
  :default-value 0
  :selector "debug"
  :type integer
  :help "Turn on debugging mode")

(define-flag *files*
  :default-value ""
  :selector "files"
  :type string
  :help "List of files to process")

(define-flag *directory*
  :default-value ""
  :selector "directory"
  :type string
  :help "Directory to process"
  :documentation "Which directory to do")

(define-flag *log-file-aux*
  :default-value ""
  :selector "log-file-aux"
  :type string
  :help "Alternate log file component"
  :documentation "If you want to have different log file")

(define-flag *help*
  :default-value nil
  :selector "help"
  :documentation "Show options"
  :type boolean)

(defparameter *command-line* nil)

(defparameter *directory-use* nil)

(defparameter *command-line-args* nil)

(defun generate-usage-string (&optional (prefix "") (suffix ""))
  (with-output-to-string (stream)
    (write-string prefix stream)
    (loop with max-flag-length =
	 (reduce #'max com.google.flag::*registered-flags* 
		 :key (lambda (x)
			(destructuring-bind (selector . flag) x
			  (+ (if (com.google.flag::boolean-flag-p flag) 2 0)
			     (length selector))))
		 :initial-value 0)
       for (selector . flag) in (reverse com.google.flag::*registered-flags*) do ;; reverse so the usage is in order of definition.
	 (format stream "~&  --~a~v@T ~A~%" selector (- max-flag-length (length selector)) (com.google.flag::help flag))
	 (when (com.google.flag::boolean-flag-p flag) ;; handle the --no<selector> for booleans.
	   (format stream "  --no~a~v@T~%"  selector (+ 2 (- max-flag-length (length selector))))))
    (write-string suffix stream)))

(defun show-opts (&optional (ign t))
  (declare (ignorable ign))
  (xlogntft "debugging ~s"  *dbg-flag* )
  (xlogntft "functions ~s" *process*)
  (xlogntft "directory ~s" *directory*)
  (xlogntft "Help ~s" *help*)
  (xlogntft "files ~s" *command-line-args*)
  (xlogntft (generate-usage-string)))

(defparameter *dispatch*
  (list (cons :scancam 'try-three-new)
		(cons :scancam-test 'try-three-alt)
		(cons :find-time-gaps 'find-time-gaps-c)
		(cons :del-dark-files 'dark-files-archive-directories)
		(cons :detect-stars 'detect-stars-new)
		(cons :delete-similar-files 'compare-directory-new) 
		(cons :end-of-day 'end-of-day-cleanup)
		(cons :end-of-day-test 'end-of-day-cleanup-test)
		(cons :subtract-dir 'subtract-dir-new)
		(cons :help 'show-opts)
		(cons :file-away-mass 'file-away-mass)
		(cons :file-away-override 'file-away-override-new)))

;; --- cheating
;; (map 'list #'(lambda (f) (com.google.flag::help (cdr f))) com.google.flag::*registered-flags*)

(defun dispatch (arg)
  (with-open-log-file ("dispatch" :show-log-file-name nil)
	(let ((*trace-output* (the-log-file)))
	  (time
	   (progn
		 (xlogntf "Dispatch, eh, and args are ~s" arg)
		 (setf *command-line* arg)
		 (if (zerop (length *directory* ))
			 (setf *directory-use* (namestring *default-pathname-defaults*))
			 (setf *directory-use* *directory*))
		 (let ((newargs (parse-command-line (rest arg))))
		   (setf *command-line-args* newargs)
		   (xlogntf "Dispach, parsed args are ~s" newargs)
		   (xlogntf "Operation is ~s" *process*)
		   (cond (*help*
				  #+nil (xlogntft "~a" (generate-usage-string))
				  (show-opts)
				  (xlogntf "help"))
				 
				 (t (let* ((ky (intern (string-upcase *process*) "KEYWORD")))
					  (let* ((func-pair (assoc ky *dispatch*))
							 (func (cdr func-pair)))
						
						(debugc 5 (xlogntft "dis: func ~s ~%func-pair is ~s ~s" *process*
											(if func-pair func-pair "")
											(if func-pair (type-of func) "")))
						(if (and func (fboundp func))
							(funcall func *command-line-args*)
							(xlogntft "Unknown operation '~a'" *process*)))))))))))
  nil)

(defun dispatch-top ()
  (dispatch sb-ext:*posix-argv*)
  (sb-ext:exit))

(defun save-core ()
  (format t "version being built is ~a with xlog version ~a~%" (slot-value (asdf:find-system 'scancam) 'asdf:version) (xlog-version))
  (sb-ext:save-lisp-and-die "scancam" 
                            :toplevel #'dispatch-top
							:save-runtime-options t
							:compression 22
                            :executable t))

;; test: (dispatch (list "sbcl" "--start=202020" "--debug=9"  "end-of-day" "help" "radio" "rescan" "delete-similar-files" "end-of-day-cleanup" ))
