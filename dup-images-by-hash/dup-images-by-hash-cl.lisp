;;;; dup-images-by-hash.lisp

(in-package #:dup-images-by-hash)

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0)))


(defun remove-duplicates-by-hash-cl ()
  (let* ((dir (uiop:native-namestring *default-pathname-defaults*))
		 (args sb-ext:*posix-argv*)
		 (who (cond (args 
					 (intern (string-upcase (file-namestring (first args))) :keyword))
					(t nil))))
	(case who
	  (:dup-images
	   (with-open-log-file ((format nil "rm-dup~A" (replace-all dir "/" "-")) :dates t  :show-log-file-name nil)
		 (log-version-number "rdbhc")
		 (xlogntf "rdbhc: dir ~s args ~s who ~s" dir args who)
		 (remove-duplicates-by-hash dir)))
	  (:dup-logs
	   (with-open-log-file ((format nil "dup-logs~A" (replace-all dir "/" "-")) :dates t  :show-log-file-name nil)
		 (remove-log-duplicates-by-hash dir)))
	  (otherwise (xlogntf "dunno what you want ~s args ~s: we understand dup-images dup-logs" who args))))
  (sb-ext:exit))

(defun save-exe ()
  (sb-ext:save-lisp-and-die "dup-images" 
                            :toplevel #'remove-duplicates-by-hash-cl
							:compression 22
                            :executable t))

