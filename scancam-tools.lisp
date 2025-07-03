(in-package #:scancam)

(defparameter *home-page* nil)

(defparameter *images-pulled* 0)

(defparameter *dark-images-moved* 0)
(declaim (fixnum *dark-images-moved*))
(defparameter *subtracted-images-deleted* 0)
(defparameter *duplicate-images-deleted* 0)
(defparameter *uninteresting-files-deleted* 0)
(defparameter *astronomy-images-found* 0)
(defparameter *cameras-polled* 0)
(defparameter *version-number* (slot-value (asdf:find-system 'scancam) 'asdf:version))

(defparameter *use-handlers* nil)
(defparameter *epoch-unixepoc-offset* (- (get-universal-time) (sb-ext:get-time-of-day)))

(defparameter *all-cams* nil)

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0)))

(defun is-a-number (who) 
  "Different from numberp, as who is a string. Parse integer wouldn't help names line '202four'"
  (every 'digit-char-p who))

(defun version-number-string (for-who)
  (format nil "~a: ver ~a " for-who *version-number*))

(defun log-version-number (for-who)
  (xlogf "~a" (version-number-string for-who) ))

(defun write-sexp-lsp (lsp fn)
  (with-open-file (fo (ensure-directories-exist fn) :direction :output :if-does-not-exist :create :if-exists :supersede)
	(debugc 5 (xlogntf "wsl: writing to ~s" fn))
    (write lsp :stream fo)))

(defun empty? (s)
  "Is s nil or the empty string ?"
  (or (null s) (string-equal "" s)))

(defun emptyp (s)
  "Is s nil or the empty string ?"
  (empty? s))

(defparameter *dirs-to-visit* nil)

(defun collect-year (basename &optional (deb nil))
  (setf *dirs-to-visit* nil)
  (let* ((pndr (pathname-directory basename))
		 (basename-last (car (last pndr))))
	(xlogntf "basename is ~s pndr ~s" basename-last pndr)
    (uiop:collect-sub*directories 
	 basename 
	 #'(lambda (dir)
		 (let* ((pnd (pathname-directory dir))
				(lst (car (last pnd))))
		   (if deb (xlogntft "first lambda, pnd is ~s, last ~s basename-last is ~s" pnd (last pnd) basename-last))
		   (if deb (xlogntf "directory is ~a and last is ~a" pnd lst))
		   (if deb (xlogntf "directory is ~a nuumber"
						   (if (is-a-number lst) "" "not")))
		   (or (string= lst basename-last)
			   (is-a-number (car (last pnd))))))
	 t
	 #'(lambda (dir) 
		 (let* ((pnd (pathname-directory dir))
				(revpnd (reverse pnd))
				(day (first revpnd))
				(month (second  revpnd))
				(year (third revpnd)))
		   (if deb (xlogntf "year ~s month ~s day ~s" year month day))
		   (when (and (= 2 (length day)) (= 2 (length month)) (= 4 (length year))
					  (is-a-number year) (is-a-number month) (is-a-number day))
			 (if deb (xlogntf "collecting ~s" dir))
			 (push dir *dirs-to-visit*))))))
  (reverse *dirs-to-visit*))

(defun yesterday ()
  (let* ((yest (adjust-timestamp (local-time:now)  (offset :day -1)))
		 (ans (format nil "~4,'0d/~2,'0d/~2,'0d"
					  (timestamp-year yest)
					  (timestamp-month yest)
					  (timestamp-day yest))))
	ans))

(defun mtime (fi)
  (sb-posix:stat-mtime (sb-posix:stat fi)))


(defun time-spread (dir)
  (let* ((whose (directory (concatenate 'string dir "*.jpg")))
		 (previous nil)
		 (delta-hash (make-hash-table :test 'equal))
		 (times nil))
	(setq whose (sort whose #'(lambda (a b)
								(string> (namestring a) (namestring b)))))
	(dolist (fx whose)
	  (push (mtime fx) times))
	(setq previous (first times))
	(setq times (sort times '> ))
	(dolist (fx (rest times))
	  (let* ((diff (- previous fx)))
		(setf (gethash diff delta-hash ) (1+ (gethash diff delta-hash 0)) )
		(xlogntf "prev ~a cur ~a dif ~a" previous fx  diff)
		(setf previous fx)))
	(maphash #'(lambda (k v)
				 (xlogntf "delta ~a count ~a" k v))
			 delta-hash)))


(defun sanitize-slashes (which)
  (replace-all (replace-all (replace-all (replace-all which "/" "_") "." "-") "&" ",") "?" "q"))

(defun find-tags (tag doc &optional (accum nil))
  "crude attempt to find tags.
   given a parsed document in S-expressions, accumulate a list of tags "
  (cond ((null doc)
         accum)
        
        ((atom doc)
         accum)
        
        ((consp doc)
         (if (equal (first doc) tag)
             (pushnew doc accum))
         (find-tags tag (car doc) (find-tags tag (cdr doc) accum)))))

(defun the-tag (element)
  "Return the tag as an atom, without attribute"
  (if (consp element)
	  (if (consp (first element))
		  (first (first element))
		  (first element))
	  element))

(defun the-tag-p (element want)
  (eq (the-tag element) want))

(defun the-attribute (element)
  (if (consp element)
	  (if (consp (first element))
			(rest (first element))
			nil)
	  element))

(defun find-cameras-link (pg)
  (cond ((null pg)
		 nil)
		
		((not (consp pg))
		 nil)
		
		((the-tag-p pg :td)
		 (let* ((link (find-tags :a pg))
				(dest (getf link :href))
				(which (second link)))
		   (break "link ~s dest ~s which ~s" link dest which)
		   which))
		
		
		(t (find-cameras-link (rest pg)))))


(defun slashes-to-hyphens (str)
  (let* ((ans (replace-all (uiop:native-namestring str) "/" "-"))
		 (l-1 (1- (length ans))))
	(if (char= #\- (char ans l-1))
		(subseq ans 0 l-1)
		ans)))

(defun file-format-time (&optional (suffix "")) 
  (let ((suf (if (and suffix (not (string= "" suffix)))
				 "_" "")))
	(multiple-value-bind(s min h d m y)
		(decode-universal-time (+ *epoch-unixepoc-offset* (sb-ext:get-time-of-day)) 0)
      (format nil "~4,'0D-~2,'0D-~2,'0D-~2,'0D-~2,'0D-~2,'0D~a~a" y m d h min s suf suffix))))

