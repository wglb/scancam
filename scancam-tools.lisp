(in-package #:scancam)

(defparameter *home-page* nil)

(defparameter *images-pulled* 0)

(defparameter *dark-images-deleted* 0)
(declaim (fixnum *dark-images-deleted*))
(defparameter *subtracted-images-deleted* 0)
(defparameter *duplicate-images-deleted* 0)
(defparameter *uninteresting-files-deleted* 0)
(defparameter *astronomy-images-found* 0)
(defparameter *cameras-polled* 0)
(defparameter *version-number* (slot-value (asdf:find-system 'scancam) 'asdf:version))

(defparameter *use-handlers* nil)
(defparameter *epoch-unixepoc-offset* (- (get-universal-time) (sb-ext:get-time-of-day)))
(defparameter *images-by-camera* nil)

(defparameter *all-cams* nil)



(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0)))

(defparameter *dirs-to-visit* nil)

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

#+nil (defun digitp (s)
  "Return t if `s' contains at least one character and all characters are numerical. Stolen from :str package"
  (unless (emptyp s)
    ;; regex ? Check sign and exponents.
    (every (lambda (char)
             (digit-char-p char))
           s)))

(defun collect-year (basename &optional (deb nil)) ;; TODO -- move to general *.lisp
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
