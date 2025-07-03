(in-package #:scancam)

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0)))

(defun my-generate-usage-string (&optional (prefix "") (suffix ""))
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
  (xlogntft (my-generate-usage-string)))

;; Process group
(defparameter *dispatch*
  (list (cons :scancam 'try-three)
		(cons :scancam-test 'try-three-alt)
		(cons :find-time-gaps 'find-time-gaps-c)
		(cons :del-dark-files 'dark-files-archive-directories)
		(cons :detect-stars 'detect-stars-new)
		(cons :delete-dark-files 'move-prod-darkfiles)
		(cons :delete-similar-files 'compare-directory-new) 
		(cons :end-of-day 'end-of-day-cleanup)
		(cons :reset-camera-counts 'reset-camera-counts)
		(cons :end-of-day-test 'end-of-day-cleanup-test)
		(cons :subtract-dir 'subtract-dir-new)
		(cons :dark-files-archive 'dark-files-archive)
		(cons :help 'show-opts)
		(cons :file-away-mass 'file-away-mass)
		(cons :file-away-auxiliary-mass 'file-away-auxiliary-mass)
		(cons :file-away-override 'file-away-override-new)))

;; --- cheating
;; (map 'list #'(lambda (f) (com.google.flag::help (cdr f))) com.google.flag::*registered-flags*)

(defun chk-for-trigger ()
  (trigger-file-hard "scancam"))

(defun time-chk ()
  (multiple-value-bind (s min h d m y)
	  (decode-universal-time (get-universal-time))
	(declare (ignorable d m y))
	(list h min s)))

#+nil (defun run-till-trigger (&optional (interv 300))
  (with-open-log-file ("scancam-trigger")
	(let ((stopping nil)
		  (*trace-output* (the-log-file)))
	  (time
	   (do ((trig (chk-for-trigger) (chk-for-trigger)))
		   ((or trig stopping))
		 (xlogntf "sleeping ~a seconds" interv)
		 (unless (try-three)
		   (xlogntft "scan fails")
		   (setf stopping t))
		 (debugc 5 (xlogntft "about to sleep; stopping is ~a" stopping))
		 (block waiting
		   (unless stopping
			 (dotimes (tx interv)
			   (if (chk-for-trigger)
				   (return-from waiting))
			   (sleep 10))))
		 (format t ".")))
	  (xlogntf "trigger exit from sleep loop"))))

(defun dispatch (arg)
  (with-open-log-file ("dispatch" :show-log-file-name nil)
	(let ((*trace-output* (the-log-file)))
	  (time
	   (progn
		 (xlogntf "Dispatch, eh, and args are ~s" arg)
		 (setf *command-line* arg)
		 (if (zerop (length *directory*))
			 (setf *directory-use* (namestring *default-pathname-defaults*))
			 (setf *directory-use* *directory*))
		 (let ((newargs (parse-command-line (rest arg))))
		   (setf *command-line-args* newargs)
		   (debugc 5 (xlogntf "dis: dbg: Dispach, parsed args are ~s, options are ~s" newargs (show-opts)))
		   (xlogntf "Operation is ~s" *process*)
		   (cond (*help*
				  (xlogntft "~a" (my-generate-usage-string))
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

(defun save-core-compressed ()
  (format t "version being built is ~a with xlog version ~a~%" (slot-value (asdf:find-system 'scancam) 'asdf:version) (xlog-version))
  (sb-ext:save-lisp-and-die "scancam" 
                            :toplevel #'dispatch-top
							:save-runtime-options t
							:compression 22
                            :executable t))

(defun save-core ()
  (format t "version being built is ~a with xlog version ~a~%" (slot-value (asdf:find-system 'scancam) 'asdf:version) (xlog-version))
  (sb-ext:save-lisp-and-die "scancam" 
                            :toplevel #'dispatch-top
							:save-runtime-options t
                            :executable t))

;; test: (dispatch (list "sbcl" "--start=202020" "--debug=9"  "end-of-day" "help" "radio" "rescan" "delete-similar-files" "end-of-day-cleanup" ))
