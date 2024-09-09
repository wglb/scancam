;;;; dup-images-by-hash.lisp

(in-package #:dup-images-by-hash)

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0)))


;; see file:///home/wgl/quicklisp/dists/quicklisp/software/ironclad-v0.45/doc/ironclad.html#digests


(defparameter *sha1-hash-table* nil)

(defparameter *errors-encountered* 0)

(defparameter *files-checked* 0)

(defparameter *version-number* (slot-value (asdf:find-system 'dup-images-by-hash) 'asdf:version))

(defun log-version-number (for-who)
  (xlogf "~a: ver ~a " for-who *version-number*))

(defun create-hash-table ()
  (make-hash-table :test 'equal :size 800))

(defun insert-hash (hash-and-date fn)
  (incf *files-checked*)
  (let* ((hash (first hash-and-date))
         (res (gethash hash *sha1-hash-table* nil)))
    (push (cons fn (cdr hash-and-date)) res)
    (setf (gethash hash *sha1-hash-table*) res)))

(defun simple-digest (fn)
  ;; TODO - do we really need the file write date?
  (if (probe-file fn)
	  (with-open-file (fi fn :element-type '(unsigned-byte 8)) 
		(let ((dig (digest-stream :sha1 fi))
			  (thehash ""))
		  (dotimes(ix (length dig))
			(setf thehash (concatenate 'string thehash (format nil "~2,'0x" (aref dig ix )))))
		  (cons thehash (file-write-date fi))))))

(defun file-hashes (list-of-files)
  (handler-case
		(progn 
		  (setf *sha1-hash-table* (create-hash-table))
		  (dolist (fn list-of-files)
			(let ((dig (simple-digest fn)))
			  (insert-hash dig fn)))
		  (let ((element-count (hash-table-count *sha1-hash-table*))
				(directory-name (directory-namestring
								 (if (first list-of-files)
									 (first list-of-files)
									 "no files!!"))))
			(debugc 5 (xlogntf "fh: hash has ~a elements for directory ~a" 
							   element-count 
							   directory-name))
			(when (plusp element-count)
			  (with-open-file (fo "unfiled-directory-count.txt" :direction :output :if-exists :append :if-does-not-exist :create) ;; TODO AHA!!!! here is the clprit. Please fix this
				(format fo "~a dir ~a~%" element-count directory-name)
				(debugc 5 (xlogntf "~a dir ~a" element-count directory-name))))
			(cons element-count directory-name)))
	  (error (qhwat)
		(incf *errors-encountered*)
		(xlogntf "fh: oopsie ~a dir is ~a" qhwat list-of-files))))

(defun car-string (el)
  (uiop:native-namestring (car el)))

;; TODO this is doubling up concepts. being used to copy a file to a directory with new file name.
;; logic not supporting that

(defparameter *deleting-not-moving* nil)

(defun exam-hashes-for-dupe ()
  (let ((delete-count 0))
    (handler-case
		(maphash #'(lambda (k v)
					 (let ((v2 (sort v 'string< :key #'car-string ))
						   (deleteme nil))
					   
					   (debugc 5
							   (when (> (length v2) 1)
								 (debugc 5 (xlogntf "ehfd: we have identical images"))
								 (debugc 5 (dolist (li v2)
											 (xlogntf "   ~a" (car li))
											 (xlogntf "ehfd: hash ~a" k)))))
					   
					   (dolist (vx  v2)
						 (when deleteme
						   (incf delete-count)
						   (debugc 5 (xlogntf "ehfd: deleting ~a" (car vx)))
						   (if *deleting-not-moving*
							   (delete-file (car vx))
							   (move-file-to-delete (car vx) "delete-duplicates")))
						 (setf deleteme t))))
				 *sha1-hash-table*)
	  (error (e)
		(incf *errors-encountered*)
		(debugc 5 (xlogntf "ehfd: exam-hashes-for-dupe botch~%    ~a" e))
		(debugc 5 (maphash #'(lambda (k v)
							   (xlogntf "k ~a v ~a" k v))
						   *sha1-hash-table*))))
	(xlogntf "ehfd: deleted ~a duplicate files from ~a files" delete-count   *files-checked*)
	delete-count))

(defun make-list-files (full-list files)
  (dolist (ix full-list)
	  (when (or (equal (pathname-type ix) "jpg")
				(equal (pathname-type ix) "jpg~"))
		(pushnew ix files)))
  files)

(defun remove-duplicates-by-hash (cl)
  (setf *errors-encountered* 0)
  (setf *files-checked* 0)
  (let ((files nil)
		(all-files nil))
	
	(file-hashes
	 (cond ((consp cl)
			(dolist (ix cl)	;; TODO--this is not tested
			  (setf files (make-list-files (directory ix) files)))
			(setf all-files files)
			files)
		   
		   ((not (null cl))
			(setf all-files (make-list-files (uiop:directory-files cl) files)))
		   
		   (t 
			(setf all-files (make-list-files (uiop:directory-files (merge-pathnames "")) files)))))
	
	(let ((ans (exam-hashes-for-dupe)))
	  (if (plusp ans)
		  (xlogntf "rdbh: Deleted ~a out of ~a images from ~a" ans (length all-files) cl))
	  ans)))

(defun remove-log-duplicates-by-hash (dir)
  (setf *deleting-not-moving* t)
  (setf *files-checked* 0)
  (file-hashes (uiop:directory-files (uiop:native-namestring dir)))
  (let ((ans (exam-hashes-for-dupe)))
	(xlogntft "rldbh: ~a duplicates deleted from ~s" ans dir)
	ans))

