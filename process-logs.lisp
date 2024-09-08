(in-package #:scancam)

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0)))



(defun f-logdir (basename)
  (let* ((basep (uiop:ensure-directory-pathname basename))
		 (basename-last (car (last (pathname-directory basep ))))
		(log-dirs nil))
	
	(xlogntf "basename is ~s basep is ~s" basename-last basep)
    (uiop:collect-sub*directories
	 basep
	 #'(lambda (dir)
		 (let* ((pnd (pathname-directory dir))
				(lst (car (last pnd))))
		   (or (string= lst basename-last)
			   (string= lst "logs"))
		   t))
	 
	 
	 t
	 #'(lambda (dir) 
		 (when (string= "logs" (car (last (pathname-directory dir))))
		   (debugc 5 (xlogntf "collecting ~a" dir))
		   (push dir log-dirs))))
	
	(reverse log-dirs)))

(defun f-logs (basename)
  (dolist (lx (f-logdir basename))
	(xlogntf "logs found~a" (directory lx))))


(defun prefix-with-file-time (logname)
  "answer the mtime (formatted) of a file"
  (if (probe-file logname)
	  (let ((seconds (sb-posix:stat-mtime (sb-posix:stat logname))))
		(multiple-value-bind (s min h d m y)
			(decode-universal-time (+ *epoch-offset* seconds))
		  (declare (ignorable s))
		  (format nil "~4,'0D-~2,'0d-~2,'0d-~2,'0d-~2,'0d-logname" y m d h min )))
	  nil))

(defun rename-with-dsfn (logname)
  (let ((nn (prefix-with-file-time logname)))
	(if nn
		(move-file-to-destination logname nn))))


;; To get logs from main camera directory 
;; (directory  (uiop:merge-pathnames* #P "*.log" "/home/data6/webcams/pendroy/scancam/US-2-@-Stateline/"))
;; To get logs from subdirectory of main camera 
;; (directory  (uiop:merge-pathnames* #P "*.log" "/home/data6/webcams/pendroy/scancam/US-2-@-Stateline/logs/"))
;; trailing slash optional


