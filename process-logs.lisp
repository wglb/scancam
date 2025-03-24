(in-package #:scancam)

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0)))

(defun log-files (dir)
  (let ((logfiles nil))
	(mapc #'(lambda (f)
			 (if (string= (pathname-type f) "log")
				 (push f logfiles)))
		 (directory (make-pathname :directory dir :name :wild :type :wild)))
	(reverse logfiles)))

(defun not-today (dir)
  (let ((not-todays nil)
		(today (dates-ymd :ymd)))
	(mapc #'(lambda (fil)
			  (when (not (search today (pathname-name fil) :test 'equal))
				(let ((dest (make-pathname :directory (append (pathname-directory dir) (list "logs")) :name (pathname-name fil) :type (pathname-type fil))))
				  #+nil (break "fil ~s ~%dest ~s" fil dest)
				  (move-file-to-destination fil dest))))
		  (log-files dir))
	not-todays))

#+nil(defun f-logdir (basename)
  "unsure how this helps"
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

#+nil (defun f-logs (basename)
  "Unsure how this helps"
  (dolist (lx (f-logdir basename))
	(xlogntf "logs found~a" (directory lx))))

#+nil (defun prefix-with-file-time (logname)
  "answer the mtime (formatted) of a file"
  (if (probe-file logname)
	  (let ((seconds (sb-posix:stat-mtime (sb-posix:stat logname))))
		(multiple-value-bind (s min h d m y)
			(decode-universal-time (+ *epoch-offset* seconds))
		  (declare (ignorable s))
		  (format nil "~4,'0D-~2,'0d-~2,'0d-~2,'0d-~2,'0d-logname" y m d h min )))
	  nil))

#+nil (defun rename-with-dsfn (logname)
  (let ((nn (prefix-with-file-time logname)))
	(if nn
		(move-file-to-destination logname nn))))


;; To get logs from main camera directory 
;; (directory  (uiop:merge-pathnames* #P "*.log" "/home/data6/webcams/pendroy/scancam/US-2-@-Stateline/"))
;; To get logs from subdirectory of main camera 
;; (directory  (uiop:merge-pathnames* #P "*.log" "/home/data6/webcams/pendroy/scancam/US-2-@-Stateline/logs/"))
;; trailing slash optional


