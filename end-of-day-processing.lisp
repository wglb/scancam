(in-package #:scancam)

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0)))

(defvar *end-of-day-cleanup*)

(defun end-of-day-cleanup-test (args)
  (xlogntft "end-of-day-cleanup-test, args are ~s" args))

(defun cleanup-one-camera (camera-directory)
  "Remove duplicates and similar images. To be done before items filed away"
  (with-open-log-file ("end-of-day-cleanup" :show-log-file-name t :dir `(:relative ,camera-directory))
	(let ((*trace-output* (the-log-file)))
	  (time
	   (progn
		 (remove-duplicates-by-hash camera-directory)
		 (dolist (subdir (list "delete-similar" "delete-darkness" "marked-images" "bright" "delete-uninteresting-new" "delete-uninteresting"))
		   (let ((newpn (make-pathname :directory (append (list :relative camera-directory) (list subdir))))) ;; 
			 (xlogntf "eodc: Going to ~s for deletion" newpn)
			 (remove-duplicates-by-hash newpn)))
		 (if (string= (get-config-rescan camera-directory :eod-processing) (machine-instance))
			 (compare-directory camera-directory)))))))

(defun cleanup-all-cameras (cams)
  "Remove dups and similars for all cameras"
  (xlogntf "cac: there are ~a cameras to process" (length cams))
  (file-away-auxiliary-mass cams)
  (mapc #'(lambda (camera-directory)
			(cleanup-one-camera (car camera-directory)))
		cams))

(defun test-cleanup-all-cameras (cams)
  "Remove dups and similars for all cameras"
  (xlogntf "cac: there are ~a cameras to process" (length cams))
  (mapc #'(lambda (camera-directory)
			(xlogntf "cleaning ~s" (car camera-directory)))
		cams)
  nil)

(defun end-of-day-cleanup (args)
  "Clean up similar images, duplicate images, and file away many things."
  (with-open-log-file ("end-of-day-cleanup" :show-log-file-name t)
	(restore-config-file-list)
	(log-version-number "eod")
	(cond ((null args)
		   (let ((*trace-output* (the-log-file)))
			 (time
			  (cleanup-all-cameras (restore-images-by-camera))))) 
		  (t (xlogntf " eod: unexpected args, ~s; processing halted" args)))
	(xlogntf " eod: ~a errors encounterd" *errors-encountered*))
  (xlogntf " eod: ~a errors encounterd" *errors-encountered*))
