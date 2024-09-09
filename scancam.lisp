;;;; scancam.lisp

(in-package #:scancam)

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0)))

(defparameter *pendroy-base* "http://rwis.mdt.mt.gov/scanweb/Camera.asp?Pageid=Camera&Units=English&Groupid=301000&Siteid=301001&Senid=&Wxid=3011&Mapid=&DisplayClass=Java&SenType=All&HEndDate=&Zoneid=&Mode=&Sprayerid=&Dvid=&CD=9%2F12%2F2013+7%3A46%3A34+PM")

(defparameter 
    *user-agent*
  ;;"User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10.6.13; rv:10.0.2) Gecko/20100101 Firefox/10.0.2"
;;  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/74.0.3729.169 Safari/537.36"
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_6_8) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/49.0.2623.112 Safari/537.36")

;; http://rwis.mdt.mt.gov/ScanWeb/Wx/images/Vid-000629001-00-00-2014-06-29-23-34.jpg sweetgrass north
(defparameter *sweetgrass-base*
  "http://rwis.mdt.mt.gov/scanweb/Camera.asp?Pageid=Camera&Units=English&Groupid=629000&Siteid=629001&Senid=&Wxid=62911&Mapid=&DisplayClass=Java&SenType=All&HEndDate=&Zoneid=&Mode=1&Sprayerid=&Dvid=&CD=6%2F29%2F2014+5%3A50%3A20+PM")

(defun ashuffle (sequence)
  (alexandria:shuffle sequence))

(defparameter *master-index-page* nil)

(defparameter *parsed-homepage* nil)

(defparameter *home-page-parts* nil)

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
         (find-tags tag (car doc) (find-tags tag (cdr doc) accum)))

        (t (xlogf "ft: wat ~A" doc))))

(defparameter *saved-home-page* nil)

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

(defun analyze-menu-page (pg)
  (dolist (fel pg)
	(when (the-tag-p fel :html)
	  (dolist (fmx fel)
		(when (the-tag-p fmx :body)
		  (dolist (fxxx fmx)
			(declare (ignorable fxxx))
			3))))))

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

(defparameter *delete-or-not* t)
(defparameter *delete-threshold* nil)
(defparameter *delete-threshold-directory* nil)
(defparameter *delete-threshold-default* 37)


;; ================================================================================- Config files

(defparameter *all-config-files* (make-hash-table :test 'equal))

(defun save-config-file-list ()
  (if (null *all-config-files*)
	  (setf *all-config-files* (make-hash-table :test 'equal)))
  (with-open-file (fo (make-pathname :name "config-file-save" :type "lsp")
					  :direction :output :if-exists :supersede :if-does-not-exist :create)
	(let ((all-configs nil)
		  (*print-right-margin* 120))
	  (maphash #'(lambda (k v)
				   (push (list k v) all-configs))
			   *all-config-files*)
	  (write (nreverse all-configs) :stream fo)
	  (length all-configs))))

(defun restore-config-file-list ()
  (if (null *all-config-files*)
	  (setf *all-config-files* (make-hash-table :test 'equal)))
  (let* ((fn (make-pathname :name "config-file-save" :type "lsp"))
		 (pf (probe-file fn)))
	(if (or (not pf) (< (sb-posix:stat-size (sb-posix:stat pf)) 5))
		(save-config-file-list))
	(with-open-file (fi (make-pathname :name "config-file-save" :type "lsp"))
	  (dolist (cx (read fi))
		(setf (gethash (first cx) *all-config-files* ) (second cx))))))

(defun get-config-rescan (dr prop &key (debug nil))
  "get config from scancam.lsp. dr is either relative or absolute directory name. Save dir and pathname in hash"
  (let* ((dir (uiop:ensure-directory-pathname dr ))
		 (cfn (make-pathname :name "rescancam" :type "lsp"))
		 (configpn (merge-pathnames dir (make-pathname :name "rescancam" :type "lsp"))))
	(let ((ckey (car (last (pathname-directory dr)))))
	  (debugc 5 (xlogntf "gcr: setting hash: index ~s value ~s" ckey configpn))
	  (if ckey
		  (setf (gethash ckey  *all-config-files* nil) configpn)))
	(let ((ans (get-config cfn prop :dir dir :debug  debug)))
	  (if debug (xlogntft "gcr: cfn ~s ans is ~s type is ~s" cfn ans (type-of ans)))
	  (if (eql (type-of ans) 'SYMBOL)
		  (format nil "~s" ans)
		  ans))))

(defun get-darkness-threshold (cl)
  (let ((ans (get-config-rescan cl :average)))
	(if ans
		ans
		0)))

;;;; ------------------------------------------------------------------------------------------

(defun chk-for-delete-darkness (long-fn)
  "Calculate the average darkness and decide based on configuration value of threshold whether it goes to dark files. If threshold not specified, don't file dark files away"
  ;; TODO break this sucker up
  (xlogntf "cfdd: long-fn ~s" long-fn)
  (if (probe-file long-fn)
	  (handler-case
		  (let* ((average (calc-average long-fn))
				 (*print-pretty* nil)
				 (rv nil)
				 (delete-count 0)
				 (dir (directory-namestring long-fn))
				 (thresh (get-darkness-threshold dir))
				 (avg (first average))
				 (delta-sum (second average))
				 (tst-thresh (if (consp thresh) ;; TOCO review this logic
								 (first thresh)
								 thresh))
				 (deltas-thresh (if (consp thresh)
									(second thresh)
									nil))
				 (below-deltas-thresh (if deltas-thresh
										  (<= delta-sum deltas-thresh)
										  nil))
				 (below-avg-thresh (if avg
									   (<= avg tst-thresh)
									   nil)))
			(when (and average thresh)
			  ;; example (85.27225 0.0 85.27037 85.27037 85.27037 26.909585656586525d0 26.909585656586525d0 26.909585656586525d0 921600)
			  (let* ((pfn (format nil "~adarkness.txt" (dates-ymd :ym)))
					 (fn (merge-pathnames dir pfn))
					 (do-title (not (probe-file fn))))
				(with-open-file (fo fn :direction :output :if-exists :append :if-does-not-exist :create)
				  (if do-title
					  ;; Vid-000629001-00-03-2021-06-06-13-22.jpg  :      121.523,      45.931,     123.231,     121.659,     119.542,      48.743,      48.744,      63.264,  921600.000,111995860.000,
					  (format fo "~42a: ~{~14,3,a ~}~%" "file name" (list "avg" "delts" "rgb1a" "rgb2a" "rgb3a" "right dev" "down dev" "total dev" "wxh" "sum") ))
				  (write-line (format nil "~42a: ~{~14,3,f,~}" (file-namestring long-fn) average ) fo)
				  (debugc 5 (xlogntf "cfdd: darkness: ~a: ~{~f, ~}" (file-namestring long-fn) average )))))
			
			(cond #+nil ((delete-uninteresting-file long-fn)
						 (xlogntf "cfdd: boring: ~a" long-fn))
				  
				  ((or below-avg-thresh below-deltas-thresh)
				   (push (xlogntf "cfdd: gonna move ~s to darkness" long-fn) rv)
				   (detect-stars-in-file (directory-namestring long-fn) long-fn)
				   (let ((err (move-file-to-delete long-fn "delete-darkness")))
					 (cond ((zerop err)
							(incf delete-count)
							(incf *dark-images-deleted*))
						   (t (incf *errors-encountered* ))))
				   (debugc 5 (xlogntf "cfdd: ~a av=~7,3,f delta sum=~7,3,f deltas-thresh=~7,3,f avgt ~a delt ~a ~a" 
									  long-fn avg delta-sum deltas-thresh below-avg-thresh below-deltas-thresh
									  "deleted")))
				  
				  (t 
				   (debugc 5 (xlogntf "cfdd: unmatch ~a av:~7,3,f del sum,:~7,3,f deltas-thresh ~7,3,f  " long-fn avg delta-sum deltas-thresh))))
			(list average rv delete-count))
		(error (ouch)
		  (xlogntf "cfdd: Boom: ~a on cfdd for file ~s" ouch long-fn)
		  nil))))

(defun slashes-to-hyphens (str)
  (let* ((ans (replace-all (uiop:native-namestring str) "/" "-"))
		 (l-1 (1- (length ans))))
	(if (char= #\- (char ans l-1))
		(subseq ans 0 l-1)
		ans)))

(defun delete-dark-files-directory (&optional (dir "."))
  (let* ((full-dir-namestring (namestring (merge-pathnames dir)))
		 (summary nil)
		 (highest 0)
		 (lowest (expt 2 24))
		 (local-deleted 0)
		 (darkness-th (get-darkness-threshold full-dir-namestring))
		 (full nil)
		 )
	(xlogntf "full-dir-namestring is ~s" full-dir-namestring)
	(xlogntf "ddfd: We got a darkness value of ~a" darkness-th)
	(with-open-log-file ((format nil "ddarkfi-dir~a" (slashes-to-hyphens full-dir-namestring))
						 :show-log-file-name t)
	  (log-version-number "ddfd: delete-dark-files-directory")
	  (setf full (directory (concatenate 'string full-dir-namestring "/*.jpg")))
		(xlogntf "ddfd: We got a darkness value of ~a" darkness-th)
		(xlogntf "ddfd: YO! Yo again !about to open log file at directory ~a" full-dir-namestring)
		(if (and (not (consp darkness-th)) (zerop darkness-th))
			(xlogntf "ddfd: no threshold, gonna save some time")
			(dolist (nf full)
			  (incf *global-images-viewed*)
			  (let* ((answ (chk-for-delete-darkness nf))
					 (ans (first answ))
					 (text (second answ)))
				(incf local-deleted (third answ))
				(if text
					(push text summary))
				
				(if ans
					(let* ((avg (first ans)))
					  (if (and avg (< avg lowest))
						  (setf lowest avg))
					  (if (and avg (> avg highest))
						  (setf highest avg)))))))
		(xlogntf "ddfd: we have ~a images to check" (length full))
		(xlogntf "ddfd: There were ~a dark files deleted out of ~a" local-deleted (length full) ))
	(xlogntf "ddfd: low=~a high=~a" lowest highest)
	(xlogntf "ddfd: There were ~a dark files deleted" local-deleted)
	nil))

(defun dark-files-archive-directories (&optional (which "."))
  (let ((dirl (if (consp which)
				  which
				  (list which))))
	(dolist (dir dirl)
	  (with-open-log-file ("dark-files-archive" :dir `(:relative ,dir) :show-log-file-name t)
		(log-version-number "dfa: ")
		(setf *dark-images-deleted* 0)
		(delete-dark-files-directory dir)
		(dolist (dx (collect-year dir))
		  (delete-dark-files-directory dx))
		(xlogntf "darkfile viewed ~a deleted ~a" *global-images-viewed* *dark-images-deleted*))))
  (xlogntf "darkfile viewed ~a deleted ~a" *global-images-viewed* *dark-images-deleted*))

(defparameter *last-body* nil)

(defun write-image-file (long-fnr body)
  (setf *last-body* body)
  (let ((*error-output* (the-log-file))
		(long-fn (calc-collision-free-file-name long-fnr)))
	
	(xlogntf "wif: ~S" long-fn)
	(incf *images-pulled*) 
	(handler-case
		(progn
		  (ensure-directories-exist long-fn)
		  (with-open-file (fo long-fn :direction :output :if-exists :supersede
									  :element-type '(unsigned-byte 8))
			(write-sequence body fo))
		  (chk-for-delete-darkness long-fn))
	  (error (q)
		(progn (xlogf "oh, we are error, q~s" q)
			   (xlogntf "wif: botch on delete darkness for ~a~%error ~a" long-fn q))))))

(defparameter *camera-map* nil)

(defun map-camera-directory (which)
  "translate spaces to hyphen, periods and slash to underscore in camera directories"
  (replace-all (replace-all (replace-all which " " "-") "." "_") "/" "_"  ))

(defparameter *parse-stack* nil)

(defun test-all-frames ()
  (with-open-log-file ("test-all-frame" :dates nil :append-or-replace :supersede)
	(log-version-number "test-all")
	(try-three)))

(defun file-format-time (&optional (suffix "")) 
  (let ((suf (if (and suffix (not (string= "" suffix)))
				 "_" "")))
	(multiple-value-bind(s min h d m y)
		(decode-universal-time (+ *epoch-unixepoc-offset* (sb-ext:get-time-of-day)) 0)
      (format nil "~4,'0D-~2,'0D-~2,'0D-~2,'0D-~2,'0D-~2,'0D~a~a" y m d h min s suf suffix))))

(defun list-cams (which)
  (mapcar #'(lambda (wh)
			  (map-camera-directory (first wh)))
		  (let ((thing (probe-file (get-config-rescan "." which))))
			(if thing
				(with-open-file (fi thing :direction :input) (read fi))
				(progn
				  (xlogntf "lc: unable to find camera list named ~a" which)
				  nil)))))

(defun list-cams-and-directories (which)
  (let ((fn (get-config-rescan "." which)))
	(cond  ((null fn)
			(xlogntft "lcad: No camera entry for ~s: " which)
			nil)
		   ((probe-file fn)
			(with-open-file (fi fn :direction :input)
			  (read fi)))
		   (t (xlogntft "lcad: No camera file for ~s: " which)
			  nil))))

(defun all-image-directories ()
  (if (null *images-by-camera*)
	  (images-by-camera))
  (unique-camera-directories *images-by-camera*))

(defun try-one-arizona ()
  (incf *cameras-polled*)
  (let* ((summary nil)
		 (image-count 0)
		 (cams (list-cams-and-directories :arizona-cams)))
	(with-open-log-file ("rwis-arizona" :dir (list :relative "arizona") #+nil (uiop:ensure-directory-pathname  "arizona")  )
	  (log-version-number (format nil "arizona: cams are ~s" cams)) 
	  (handler-case
		  (dolist (cam cams)
			(xlogntf "toa: cam is ~s directory ~s url ~s" cam (car cam) (rest  cam))
			(get-darkness-threshold (car cam))
			(let* ((fans (dex-get (second cam) :binary t))
				   (ans (dexans-body fans))
				   (long-fn 
					 (make-pathname :name 
									(file-format-time (pathname-name (third cam)))
									:type "jpg" :defaults (pathname-as-directory (car cam)))))
			  (incf image-count)
			  (push (cons (car cam) image-count) *images-by-camera*)
			  (write-image-file long-fn ans)))
		(error (c)
		  (xlogntf "toa: botch in arizona camera fetch ~s" c)))
	  (xlogntf "toa: done with arizona"))
	summary))

(defun get-site-home ( &optional (name "glacier-park-home.lsp") (url nil))
  (let* ((home-loc 
		   (if (null url)
			   (cdr (with-open-file (fi (get-config-rescan "." :glacier-cams) :direction :input)
					  (read fi)
				 (read fi)))
			   url))
		 (dex-res (dex-get home-loc)))
	(cond (dex-res
		   (let ((doc (parse-html (dexans-body dex-res))))
			 (write-sexp-lsp doc name)))
		  (t (xlogntft "no home doc from  ~s" dex-res)))))

(defun get-one-glacier-park (pair)
  "Get one image from a glacier-park type camera"
  ;; TODO: callers should note the return value and add up
  (let ((the-directory  `( :relative ,(car pair))))
	(with-open-log-file ((format nil "~a-~a" "rwis-glacier" (car pair)) :dir the-directory  :show-log-file-name t) ;; too many  logs before :dates :hms
	  (log-version-number (format nil "get-one-glacier-park ~a" (car pair)))
	  (xlogf "g1: ==== get-one-glacier-park ~a" (car pair))
	  (incf *cameras-polled*)
	  (let* ((uri (cdr pair))
			 (rv nil)
			 (image-count 0))
		
		(handler-case
			(let ((ans (dex-get uri :binary t)))
			  (let ((long-fn (concatenate 'string ;; TODO use proper pathname stuff
										  (car pair)
										  "/"
										  (file-format-time)
										  ".jpg")))
				(debugc 5 (xlogntf "g1: ~s" long-fn))
				(if ans
					(handler-case
						(let* ((headers (dexans-headers ans))
							   (ctyp (if headers
										 (gethash "content-type" headers "no-ct")
										 "no headers")))
						  (xlogntf "g1: content type ~s" ctyp)
						  (if (string= "image/jpeg" ctyp)
							  (write-image-file long-fn (dexans-body ans))
							  (xlogntf "g1: bad content type ~s" ctyp)) 
						  (incf image-count))
					  (error (k)
						(xlogntf "g1: write error for ~a, is it length problem ~a error ~a" long-fn (length (dexans-body ans)) k))))))
		  (error (d)
			(incf *errors-encountered*)
			(setf rv (xlogntft "g1: error: glacier-oopsie on camera ~a ~%[[~a]] for <<~a>>, but carry on" (car pair) (type-of d) uri))
			(debugc 5 (with-open-file (fo (format nil "~A-glacier-oops-err" (formatted-current-time))
										  :direction :output :if-does-not-exist :create :if-exists :append)
						(write-line (format nil "g1: glacier-oopsie [[~a]] for <<~a>>, but carry on" d (first pair))
									fo)))))
		
		(xlogf "g1: ---- get-one-glacier-park ~a" (car pair))
		(push (cons (car pair) image-count) *images-by-camera*)
		(if rv
			(cons rv image-count)
			image-count))
	  (xlogntf "g1: ---- done with ~a" (car pair))))
  (xlogntf "g1: ---- done with ~a" (car pair)))

(defun get-wh-marina ()
  (let* ((ans (dex-get (uri "https://www.earthcam.com/cams/includes/image.php?logo=0&playbutton=0&s=1&img=g7tDudP%2F7yBUDfCACguaJQ%3D%3D&202308211200") :binary t))
		 (relm `(:relative "wh-marina"))
		(fn (make-pathname :name (file-format-time "wh-marina") :directory relm :type "jpg")))
	(with-open-log-file  ("marina" :dates t :dir relm)
	  (xlogf "gwm: size ~s" (length (dexans-body ans)))
	  (let* ((headers (dexans-headers ans))
			(ctyp (if headers
					  (gethash "content-type" headers "no-ct"))))
		(cond ((string= "image/jpeg" ctyp)
			   (write-image-file fn (dexans-body ans))
			   (pushnew (cons "wh-marina" 1) *images-by-camera*))
			  (t (xlogntf "gwm: Content type not image: ~s " ctyp)))))))

(defun get-all-glacier ()
  (dolist (el (list-cams-and-directories :glacier-cams))
    (let ((rv (get-one-glacier-park el)))
	  (xalertf "gallg: running ~a ~a" (first el) (version-number-string "gallg"))
	  rv)))

(defun  get-hour-minute ()
  (multiple-value-bind (s min h d m y day dstflg offset)
	  (decode-universal-time (get-universal-time))
	(declare (ignore s d m  y day ))
	(let ((zulu-hour (+ offset  
				   (if dstflg -1 0))))
	  (xlogntf "minute ~a hour ~a zulu-hour ~a" min h zulu-hour)
	  (list h min))))

(defun unique-camera-directories (all-camsx)
  (let* ((cams-hash (make-hash-table :test 'equal))
		 (all-camsd (map 'list 'first all-camsx))
		 (all-cams nil))
	(mapc #'(lambda (c)
			  (setf (gethash c cams-hash) t))
		  all-camsd)
	(maphash #'(lambda (k v)
				 (declare (ignorable v))
				 (push k all-cams))
			 cams-hash)
	all-cams))

(defun do-we-need-to-delete-duplicates (all-cams)
  "all cams is a list of a cons of camera directory name and image count"
  (let ((*trace-output* (the-log-file)))
	(time 
	 (with-open-log-file ("deleting-duplicates")
	   (let ((*trace-output* (the-log-file)))
		 (log-version-number "del dupes")
		 (dolist (fx (unique-camera-directories all-cams))
		   (with-open-log-file ("deleting-duplicates" :dir `(:relative ,fx))
			 (let ((*trace-output* (the-log-file)))
			   (time
				(let ((ans (remove-duplicates-by-hash (format nil "~a/" fx))))
				  (incf *duplicate-images-deleted* ans)
				  (xlogntf "dwdtdd: ~a duplicates deleted from ~s" ans fx ))))))
		 (xlogntf "dwdtdd: ~a duplicates deleted" *duplicate-images-deleted*)))))
  (xlogntf "dwdtdd: ~a duplicates deleted" *duplicate-images-deleted*))

(defun write-unfiled-count (all-cams)
  "TODO - make this a lsp format file"
  (with-open-file (fo "unfiled-directory-count.txt" 
					  :direction :output
					  :if-exists :supersede
					  :if-does-not-exist :create)
	(let ((the-list nil))
	  (dolist (jx all-cams)
		(let ((list-o-files (make-list-files (directory  (concatenate 'string (file-namestring jx)   "/*.jpg")) nil)))
		  (debugc 5 (xlogntf "wuc: count for ~s is ~s" jx (length list-o-files)))
		  (push (format nil "~a ~a" (length list-o-files) jx) the-list)))
	  (dolist (ix (sort the-list #'(lambda (str1 str2)
									 (< (parse-integer str1 :junk-allowed t)
										(parse-integer str2 :junk-allowed t)))))
		(write-line ix  fo)))))

(defun time-to-run ()
  (let* ((hour-minute (get-hour-minute))
		(last-run (probe-file ".last-run"))
		(last-time-run (if last-run
						   (with-open-file (fi last-run)
							 (read fi))
						   nil))
		(elapsed (if  (and last-time-run (consp last-time-run ) ) ;; TODO check if this .last-run is a list
					 (+ (- (second hour-minute) (second last-time-run)) (* 60 ( - (first hour-minute) (first last-time-run))))
					 nil))
		(time-to-run (if elapsed
						 (or (not last-time-run)
							 (> elapsed 12)
							 (not (plusp elapsed)))
						 t)))
	(xlogntf "our last run  was ~a and cur time is ~a elapsed is ~a" last-time-run hour-minute elapsed)
	(debugc 5 (if last-time-run
				  (xlogntf "t3: diff is ~a" elapsed)))
	(cond (time-to-run
		   (with-open-file (fo ".last-run" :direction :output :if-exists :supersede :if-does-not-exist :create) ;; TODO simplify this code
						(write hour-minute :stream fo)))
		  (t (xlogntf "t3: Not running RWIS this time, elapsed is ~a" elapsed)))
	time-to-run))

(defun do-kitt-peak-cams ()
  (let ((cams (list-cams-and-directories :kitt-peak-cams)))
	(dolist (el cams)
	  (xalertf "t3k: running ~a ~a" (first el) (version-number-string "t3k"))
	  (let ((rv (get-one-glacier-park el)))
		(xlogntf "t3k: done with ~a" (first el)) rv))))

(defun do-vermont-cams ()
  (dolist (el (list-cams-and-directories :vermont-cams))
	  (xalertf "t3v: running ~a ~a" (first el) (version-number-string "t3v"))
	  (let ((rv (get-one-glacier-park el)))
		rv)))

(defun vermont-cams-test ()
  (with-open-log-file ("vermont-cams-test")
	(do-vermont-cams)))

(defun try-pendroy-new ()
  (if (get-rwis-home-page "current-new") 
	  (handler-case
		  (progn
			(find-images-new-home-page *saved-home-page*)
			(with-open-file (fod "live-directories.lsp"  ;; TODO This is redundant with 'images-by-camera, but not as up to date
								 :direction :output :if-exists :supersede :if-does-not-exist :create)
			  (write (all-image-directories) :stream fod)))
		(error (e)
		  (xlogntft "tpn: error in new rwis home page processing ~s:" e)))
	  (xlogntft "tpn: home page fetch failure")))

(defun images-by-camera0 (images cfh reset)
  (dolist (ix images)
	(let ((ent (gethash (car ix) cfh (cons (car ix) (cdr ix))) ))
	  (if reset
		  (setf ent (cons (car ent) 0)))
	  (setf (gethash (car ix) cfh) ent))))

(defun images-by-camera (&key (reset nil))
  (let* ((ibc "images-by-camera1.lsp")
		 (images (if (and (probe-file ibc)
						  (plusp (sb-posix:stat-size (sb-posix:stat ibc))))
					 (with-open-file (fi ibc :direction :input)
					   (read fi))
					 nil))
		 (cfh (make-hash-table :test 'equal)))
	(images-by-camera0 images cfh reset)
	(images-by-camera0 *images-by-camera* cfh reset)
	(with-open-file (fo ibc :direction :output :if-exists :supersede :if-does-not-exist :create)
	  (maphash #'(lambda (k v)
				   (declare (ignorable k))
				   (push v *images-by-camera*))
			   cfh)
	  (write *images-by-camera* :stream fo))))

(defun try-three (&optional (alternate-log-file-name nil))
  (declare (ignorable alternate-log-file-name))
  (setf *images-by-camera* nil)
  (images-by-camera :reset nil)
  (setf *all-config-files* nil)
  (restore-config-file-list)
  (with-open-log-file ((if alternate-log-file-name
						   alternate-log-file-name
						   "try3")
					   :show-log-file-name t) ;; was :dates :hms; too many logs
	
	(set-alert-file-name "scancam")
	(log-version-number "t3:==================== scancam (try-three) ")
	(setf *images-pulled* 0)
	(setf *dark-images-deleted* 0)
	(xlogf "Begin run")
	(cond ((lockme "scancam")
		   (xalertf "t3: locked ok")
		   (xalertf "t3: running ~a" (version-number-string "t3"))
		   (let* ((*print-pretty* nil))
			 (do-kitt-peak-cams)
			 (do-vermont-cams)
			 (get-all-glacier)
			 (try-one-arizona)
			 (when (time-to-run)
			   (get-wh-marina) ;; let's calibrate the interval 
			   (try-pendroy-new)
			   (do-we-need-to-delete-duplicates *images-by-camera*))
			 (unlockme "scancam"))
		   (xalertf "img=~a drk=~a dup-rm=~a sim=~a cams=~a err=~a star=~a borg=~a v~a"
					*images-pulled*
					*dark-images-deleted*
					*duplicate-images-deleted*
					*similar-images-deleted*
					*cameras-polled*
					*errors-encountered*
					*astronomy-images-found*
					*uninteresting-files-deleted*
					(version-number-string "img")))
		  
		  (t 
		   (xalertf "t3: ~a Lock file in place!! ~a ~a" "▁██████"  (formatted-file-time "scancam,lck") (version-number-string "t3"))))
	(save-config-file-list)
	(xlogntf "t3: successful exit"))
  
  (xlogf "scancam (try-three) done ~a" (version-number-string "try-three"))
  (xlogntf "img=~a drk=~a dup-rm=~a sim=~a cams=~a err=~a star=~a borg=~a v~a"
		   *images-pulled*
		   *dark-images-deleted*
		   *duplicate-images-deleted*
		   *similar-images-deleted*
		   *cameras-polled*
		   *errors-encountered*
		   *astronomy-images-found*
		   *uninteresting-files-deleted*
		   (version-number-string "img")) 
  (images-by-camera)
  (write-unfiled-count (map 'list 'first *images-by-camera*))
  (xlogf "End of run"))

(defun try-three-alt (args)
  (cond (args
		 (xlogntf "Unexpected extra args, processing halted: ~% ~s " args))
		(t (xlogntf "Would be processing '~s'" args))))

(defun try-three-new (args)
  (cond (args
		 (xlogntf "Unexpected extra args, processing halted: ~% ~s " args))
		(t (try-three args))))

(defun try-three-with-quit ()
  (try-three)
  (sb-ext:exit))

(defun end-of-day-cleanup-test (args)
  (xlogntft "end-of-day-cleanup-test, args are ~s" args))

(defun  end-of-day-cleanup (args)
  "This needs to restore the camera list"
  (with-open-log-file ("end-of-day-cleanup" :show-log-file-name t)
	(restore-config-file-list)
	(log-version-number "eod")
	(cond ((null args)
		   (let ((*trace-output* (the-log-file))
				 (cams (all-image-directories)))
			 (time
			  (progn
				(xlogntf "eodc: there are ~a cameras to process" (length cams))
				(dolist (camera-directory cams)
				  (with-open-log-file ("end-of-day-cleanup" :show-log-file-name t :dir `(:relative ,camera-directory))
					(remove-duplicates-by-hash camera-directory)
					(dolist (subdir (list "delete-similar" "delete-darkness" "marked-images" "bright" "delete-uninteresting-new" "delete-uninteresting"))
					  (let ((newpn (make-pathname :directory (append (list :relative camera-directory) (list subdir))))) ;; TODO -- new code
						(xlogntf "eodc: Going to ~s for deletion" newpn)
						(remove-duplicates-by-hash newpn)))
					(compare-directory camera-directory)))
				(delete-uninteresting-mass )
				(file-away-auxiliary-mass)))))
		  (t (xlogntf " eod: unexpected args, ~s; processing halted" args)))
	(xlogntf " eod: ~a errors encounterd" *errors-encountered*))
  (xlogntf " eod: ~a errors encounterd" *errors-encountered*))

(defun calc-date-structered-directory-name (fx)
  "This takes a simple file name with no directory component, returns relative directory or nil if file not date-stamped"
  (let* ((fn (pathname-name fx))
		 (tokes (tokenize1 fn #\-))
		 (part tokes)
		 (basse nil)
		 (newfn nil))
	(debugc 5 (xlogntf "cdsdn: fn ~a ~%    ~a ~a" fn tokes (last tokes 5)))
	(xlogntf "cdsdn: fn ~a ~%    ~a ~a" fn tokes (last tokes 5))
	(block collector
	  (dolist (ix tokes)
		(declare (ignorable ix))
		(when (and (digitp (car part)) (= (length (car part)) 4) (not (position #\_ (car part))))
		  (setf basse part)
		  (return-from collector))
		(setf part (rest part))))
	(debugc 5 (xlogntf "cdsdn: part ~a base ~a" part basse))
	(let ((path nil))
	  (dotimes (ix 3)
		(setf path (append path (list (nth ix basse)))))
	  (if (> (length basse) 2)
		  (setf newfn
			(if basse
				(make-pathname :directory `(,@(pathname-directory fx) ,@path) :name (pathname-name fx) :type (pathname-type fx) )
				nil))))
	(debugc 5 (xlogntf "cdsdn:  new file name ~a" newfn))
	newfn))
(defun test-calc-date-structured-directory-name (&optional (elem "Aberdeen-Hill-263004-00-3-26-2024-12-15-1.jpg"))
  (calc-date-structered-directory-name (calc-path (calc-dir-from-tokens (tokenize1 elem #\-))) ))

;; (calc-date-structered-directory-name (calc-path (calc-dir-from-tokens (tokenize1 "Aberdeen-Hill-263004-00-3-26-2024-12-15-1.jpg" #\-))) )

(defun file-away-one-file (fx) 
  (let ((new-file-name (calc-date-structered-directory-name fx))
		(file-count nil))
	(if new-file-name
		(let ((err (move-file-to-destination fx new-file-name)))
		  (if (zerop err)
			  (setf file-count t))
		  (incf *errors-encountered* err)))
	file-count))

(defun file-away-list (a-list-of-files)
  "return count of files moved"
  (let ((file-count 0))
	(dolist (fx a-list-of-files)
	  (let ((ty (pathname-type fx)))
		(if (or (string= ty "jpg") (string= ty  "lsp"))
			(let ((faao (file-away-one-file fx)))
			  (if faao
				  (xlogntf "fal: moved ~s" fx)
				  (xlogntf "Whaat? we can't move ~a which has type of ~a which is ~a"
						   fx
						   (pathname-type fx)
						   (string= (pathname-type fx) "jpg"))))
			(xlogntf "Whaat? we are being asked to not move ~a which has type of ~a which is ~a"
					 fx
					 (pathname-type fx)
					 (string= (pathname-type fx) "jpg")))))
	(xlogntf "fal: moved ~a files" file-count)
	file-count))

(defun file-away-flist (the-dir)
  (append (append (directory (uiop:merge-pathnames* #P"*.jpg" (uiop:ensure-directory-pathname the-dir)))
							 (directory (uiop:merge-pathnames* #P"*.lsp" (uiop:ensure-directory-pathname the-dir) )))))

(defun file-away-directory (the-dir)
  "file away the date-stamped *.jpg and *.lsp files in the specified directory depending on the setting of :file-away"
  (with-open-log-file ((format nil "f-away-dir-~a" (slashes-to-hyphens the-dir))
					   :dir  (list :relative (car (last (pathname-directory (uiop:ensure-directory-pathname the-dir)))))
					   :show-log-file-name t)
	(let ((to-file-p (get-config-rescan the-dir :file-away)))
	  (xlogntf "fad: to file or not ~s" to-file-p)
	  (cond ((eq to-file-p :true)
			 (log-version-number "file-away")
			 (xlogntf "fad: directory is ~s fdir is ~a " the-dir the-dir )
			 (let ((list-o-files (file-away-flist the-dir)))
			   (file-away-list list-o-files)))
			  
			(t 
			 (xlogntf "fad: not to file these for ~a" the-dir))))))

(defun file-away-auxiliary (camera-directory)
  "File away contents of major directory and sub directories"
  (xlogntft "file-away-aux ~s" camera-directory)
  (cond ((probe-file camera-directory)
		 (dolist (dx (list "delete-similar" "delete-duplicates" "delete-darkness" "marked-images" "bright" "radio" "delete-uninteresting-new" "delete-uninteresting"))
		   (let ((newpn (make-pathname :directory (append (list :relative camera-directory) (list dx))) #+nil (concatenate 'string camera-directory "/" dx))) ;; use the UIOP one
			   (xlogntf "~s" newpn)
			   (if (not newpn)
				   (xlogntf "No path for ~s" dx)
				   (file-away-directory newpn)))
		   (file-away-directory (make-pathname :directory (list :relative camera-directory))))
		 t)

		(t (xlogntf "No base directory ~a" camera-directory)
		   nil)))

(defun file-away-auxiliary-mass ()
  (with-open-log-file ("file-aux")
	(dolist (dx (all-image-directories))
	  (with-open-log-file ("file-aux" :dir `(:relative ,dx) :show-log-file-name t)
		(file-away-auxiliary dx) ;; TODO this doesn't quite do it.
		(remove-duplicates-by-hash dx)))))

(defun file-away-override (args)
  "file away the files in this list regardless of the parameter"
  (file-away-list args))

(defun file-away-override-new (args)
  (file-away-list args))

(defun file-away-mass (&optional (directories (map 'list 'first *images-by-camera*))) ;;(map 'list 'first *images-by-camera*)
  "File away all *.jpg *.lsp (date-stamped) in each of the directories in the list 'directories'"
  (with-open-log-file ("file-away-mass" :show-log-file-name t)
	(log-version-number "file-away-mass:")
	(let ((dirs directories))
	  (dolist (dx dirs)
		(file-away-directory dx)))))

#+nil(defun delete-uninteresting-file (longfn)
  longfn
  nil)

(defun delete-uninteresting-file-old (longfn)
  "determine if this is an uninteresting file. if so, move the sucker. Answer true if moved"
  ;; TODO finish.
  (let* ((dir (directory-namestring longfn))
		 (bad-patterns (get-config-rescan dir :not-interesting))
		 (dc-fn (string-downcase (file-namestring longfn)))
		 (ans nil))
	(cond ((null bad-patterns)
		   nil)
		  ((consp bad-patterns)
		   (dolist (px bad-patterns)
			 (when (search (string-downcase px) dc-fn)
			   (when (move-file-to-delete longfn "delete-uninteresting-new")
				 (incf *uninteresting-files-deleted*)
				 (setf ans t)))))
		  (t
		   (when (search (string-downcase bad-patterns) dc-fn)
			 (when (move-file-to-delete longfn "delete-uninteresting-new")
			   (incf *uninteresting-files-deleted*)
			   (setf ans t)))))
	ans))

(defun delete-uninteresting (dir)
  dir)

(defun delete-uninteresting-old (dir)
  "Delete uninteresting files from directory. TODO: this logic really sucks"
  (let* ((fancy-name (format nil "dun-~a" (slashes-to-hyphens dir)))
		 (regexp-uninteresting nil)
		 (bad-patterns (get-config-rescan dir :not-interesting))
		 (uninteresting-files-deleted 0))
	(when bad-patterns
	  (with-open-log-file (fancy-name :dir (list :relative dir) :show-log-file-name t)
		(log-version-number dir)
		(xlogntf "du: for directory ~s ~%   we have ~s" dir bad-patterns)
		(cond ((null bad-patterns)
			   (xlogntf "du: no bad-patterns to check"))
			  
			  ((consp bad-patterns)
			   (dolist (px bad-patterns)
				 (if (eq (type-of px)  'symbol)
					 (push (format nil "~a" px) regexp-uninteresting)
					 (push px regexp-uninteresting))))
			  
			  (t (push bad-patterns regexp-uninteresting)))
		
		(when regexp-uninteresting
		  (let ((jpg-files (directory (concatenate 'string dir "*.jpg"))))
			(dolist (jx jpg-files)
			  (let ((jxs (file-namestring jx)))
				;; could be as follows
				;; (delete-uninteresting-file jxs regexp-uninteresting)
				(dolist (re  regexp-uninteresting)
				  (when (search (string-downcase re) (string-downcase jxs))
					(let ((err (move-file-to-delete jx "delete-uninteresting")))
					  (incf *errors-encountered* err)
					  (when (zerop err)
						(incf *uninteresting-files-deleted*)
						(incf uninteresting-files-deleted)))))))))
		
		(xlogntf "du: deleted ~a uninteresting files in ~a" 
				 (if (zerop uninteresting-files-deleted)
					 "no"
					 uninteresting-files-deleted)
				 dir))
	  (xlogntf "du: deleted ~a uninteresting files in ~a" 
			   (if (zerop uninteresting-files-deleted)
				   "no"
				   uninteresting-files-deleted)
			   dir))
	fancy-name))

(defun delete-uninteresting-archives (site-directory)
  "Delete uninteresting files from top-level directiry ndir"
  (with-open-log-file ("dua-arch" :show-log-file-name t)
	(log-version-number site-directory)
	(xlogntf "dua: dirs ~a" (probe-file  site-directory))
	(delete-uninteresting site-directory)
	(dolist (yr (collect-year site-directory))
	  (delete-uninteresting yr))
	(xlogntf "deleted ~a uninteresting files"
			 (if (zerop *uninteresting-files-deleted*) "no" *uninteresting-files-deleted*)))
  (xlogntf "deleted ~a uninteresting files"
		   (if (zerop *uninteresting-files-deleted*) "no" *uninteresting-files-deleted*)))


(defun delete-uninteresting-mass ()
  )

(defun delete-uninteresting-mass-old ()
  (setf *uninteresting-files-deleted* 0)
  (with-open-log-file ("delete-uninteresting" :show-log-file-name t)
	(log-version-number "uninteresting-mass")
	(if (probe-file  "live-directories.lsp")
		(let ((dirs (with-open-file (fi "live-directories.lsp")
					  (read fi))))
		  (dolist (dx dirs)
			(xlogntf "dum: dir ~a  says" dx)
			(delete-uninteresting dx)))
		(xlogntf "dum: lol nope"))
	(xlogntf "deleted ~a uninteresting files" *uninteresting-files-deleted*))
  (xlogntf "deleted ~a uninteresting files" *uninteresting-files-deleted*))

(defvar *scancam*)

(defvar *scancam-test*)

(defvar *detect-stars*)
(defvar *delete-similar-files*)
(defvar *end-of-day-cleanup*)
(defvar *subtract-dir*)
(defvar *file-away*)
(defvar *file-away-override*)
(defvar *file-away-archive*)
(defvar *delete-similar-files-archives*)
(defvar *delete-dark-files*)
(defvar *delete-uninteresting*)
(defvar *delete-uninteresting-archives*)


(defparameter *camera-home-page* nil)

(defun get-rwis-home-page (&optional (base "") (page "https://app.mdt.mt.gov/atms/public/cameras"))
  ;; beginning of group "((:DIV :CLASS "modal camera-modal rwis-modal" :ID "modal2")"
  ;; group header for camers is                   ((:DIV :CLASS "col-md-12 col-lg
  ;; title of camera group                 ((:H4 :CLASS "modal-title") "Baker") 
  ;; no other use of the h4 sequence above
  ;; individual image :IMG :TABINDEX "1" :CLASS "img-fluid" :SRC
  ;; reference image would be 
  ;; ((:IMG :TABINDEX "1" :CLASS "img-fluid" :SRC
  ;;                     "https://mdt.mt.gov/other/WebAppData/External/RRS/RWIS/Pendroy-301001-00-3-1-2024-15-15-1.jpg"
  ;;                    :ALT "South Elev 4199 - 03/01/2024 03:15 PM"
  ;;                     :DATA-POSITIONID "301010" :DATA-POLL-DATE
  ;;                     "03/01/2024 03:15 PM"))
  ;; four pendroy (should be four) under img: :class "img-fluid". others imply thumb and some with js.
  ;; key images always have :tabindex element.
  
  (let* ((ans (dex-get page))
		 (pbody (dexans-body ans))
		 (*print-pretty* t))

	(cond ((dexans-err ans)
		   (xlogntft "bad html from home page, counting on saved")
		   (restore-rwis-home-page base))

		  (t (let ((ppbody (parse-html pbody)))
			   (setf *saved-home-page* (write-sexp-lsp ppbody (format nil "rwis-cameras-new-~a.lsp" base))))))) 
  
  (length *saved-home-page*))

(defun restore-rwis-home-page (&optional (base ""))
  (with-open-file (fi (format nil "rwis-cameras-new-~a.lsp" base))
	(setf *saved-home-page* (read fi)))
  (length *saved-home-page*))


(defparameter  *collected-bits* nil)

(defun cons-form-p (form &optional (test #'keywordp))
  (and (consp form)
       (or (funcall test (car form))
           (and (consp (car form))
				(funcall test (caar form))))))

;;:DIV :STYLE "max-width:1280px;" :CLASS "img-caption mb-0 mx-auto text-right text-white h6"

(defun finhp (elem)
  "Traverse the new pendroy home page, finding relevant images"
  (cond ((cons-form-p elem)
		 (let* ((fi (car elem))
				(tag (if (listp (car elem))
						 (caar elem)
						 nil)))
		   
		   (when (equal tag :img)
			 (debugc 5 (xlogntf  "We have a div element, second is  ~s:" (cdar elem )))
			 (when (and (equal (second fi) :tabindex)
						(equal (third fi)  "1" )
						(equal (fifth fi) "img-fluid"))
			   (debugc 5 (xlogntf "are we there yet ~s with image ~s" fi (getf (rest fi) :src)))
			   (debugc 5 (xlogntf "we have the tag ~s" fi))
			   (let* ((rfi (rest fi))
					  (found (list (getf rfi :src) (getf rfi :alt) (getf rfi :data-positionid))))
				 (push found *collected-bits*))))
		   (if (equal (car elem) :img)
			   (break "we gotta img ~s" (rest elem)))
		   (dolist (el (rest elem))
			 (finhp el))))

		((not (consp elem))
		 #+nil (break "not consp elem ~s" elem))
		
		((not (cons-form-p elem))
		 (xlogntf "not cons form ~s" elem))
		
		(t (xlogntf "not consp for ~s" elem))))

(defun pull-rwis (base)
  (incf *cameras-polled*)
  (handler-case
	  (let* ((full (car base))
			 (uri (cdr base)))

		(xlogf "tod: ~%====================~%  base ~s~%  ~s url ~s" base full uri)
		(ensure-directories-exist full) 
		(let* ((ans (dex-get uri :binary t))
			   (body (dexans-body ans))
			   (headers (dexans-headers ans)))
		  (xlogntf "content type ~s" (if headers
										 (gethash "content-type" headers "whoops")
										 "double whoops"))
		  
		  (setf *last-body* body)
		  (handler-case
			  (write-image-file full body)
			(error (e)
			  (xlogntf "pw: botch ~s on ~s, header type ~s" e uri
					   (if headers
						   (gethash "content-type" headers "whoops")
						   "double whoops"))))))
	(error (q)
	  (xlogntft "pull-rwis barf on base ~s error ~s cameras ~s" base q *cameras-polled*))))

(defun calc-path (fn-tokesa)
  (let* ((dir (first fn-tokesa))
		 (fn-tokes (second fn-tokesa))
		 (camera-id (first fn-tokes))
		 #+nil (extra (parse-integer(second fn-tokes)))
		 (extra (second fn-tokes))
		 (year (parse-integer (fifth fn-tokes)))
		 (month (parse-integer (third fn-tokes)))
		 (day (parse-integer (fourth  fn-tokes)))
		 (hour (parse-integer (sixth fn-tokes)))
		 (minute (parse-integer(seventh fn-tokes)))
		 (extra2 (tokenize1 (eighth fn-tokes) #\.))
		 (typ (second extra2))
		 (aux (first extra2))
		 (fmt (list
			   (cons "~a-"     camera-id)
			   (cons "~a-"      extra)
			   (cons "~4,'0D-" year )
			   (cons "~2,'0D-" month)
			   (cons "~2,'0D-" day)
			   (cons "~2,'0D-" hour)
			   (cons "~2,'0D-" minute)
			   (cons "~2,'0D" aux)
			   ))
		 (ofn (format nil "~4,'0D-~2,'0D-~2,'0D-~2,'0D-~2,'0D-~2,'0D-~a-~a"
					  year month day
					  hour minute aux
					  camera-id
					  extra))
		 (form nil))
	ofn
	(dolist (fx fmt)
	  (push (format nil (car fx) (cdr fx) ) form))
	
	(let* ((fn (format nil "~{~a~}" (nreverse form)))
		   (pfn (make-pathname 
				 :directory  `(:relative ,dir)
				 :name fn	
				 :type typ)))
	  (format nil "~{~a-~}" (rest fn-tokes))
	  pfn)))
;; (calc-path (calc-dir-from-tokens (tokenize1 "Aberdeen-Hill-263004-00-3-26-2024-12-15-1.jpg" #\-)))
;; Aberdeen-Hill-263004-00-3-26-2024-12-15-1.jpg

(defun calc-dir-from-tokens (tokes)
  (if (equal 9 (length tokes))
	  (list (first tokes) (rest tokes))
	  (if (equal 10 (length tokes))
		  (list (format nil "~a-~a" (first tokes) (second tokes))  (rest (rest tokes)))
		  (if (equal 11 (length tokes)) 
			  (list (format nil "~a-~a-~a" (first tokes) (second tokes) (third tokes))  (rest (rest (rest tokes))))
			  (break "unknown length of ~a for ~s" (length tokes) tokes)))))

(defun pull-new-rwis-image (which)
  "Pull the new style rwis images; answer the directory"
  (let* ((the-url (first which))
		 (path  (uri-path (uri the-url)))
		 (tokes (tokenize1 (file-namestring path) #\-))
		 (dirx (calc-dir-from-tokens tokes))
		 (pfn (calc-path dirx)))
	#+nil (break "which  ~s~%uri    ~s~%dir   ~s fn ~s~%" which  path dirx pfn)
	(with-open-log-file ((format nil "~a-~a" (first dirx) "rwis") :dir `(:relative ,(first dirx)))
	  (pull-rwis (cons pfn (uri the-url))))
	(first dirx)))

;; (calc-dir-from-tokens (tokenize1 "Aberdeen-Hill-263004-00-3-26-2024-12-15-1.jpg" #\-))

(defun find-images-new-home-page (lsp)
  (images-by-camera)
  (setf *collected-bits* nil) 
  (let ((*print-pretty* nil))
	(log-version-number "finhp")
	(finhp lsp)
	(setf *collected-bits* (ashuffle *collected-bits*))
	(dolist (cb *collected-bits*)
	  (pushnew (cons (pull-new-rwis-image cb) 1) *images-by-camera*))) ;; todo does not account for multiple cameras on given site
  (with-open-file (fo "collected.bits.lsp" :direction :output :if-exists :supersede :if-does-not-exist :create)
	(let ((*print-pretty* t))
	  (write *collected-bits* :stream fo))))

(defun find-time-gaps-c (arg)
  (let ((argx 
		  (if (null arg)
			  (list *default-pathname-defaults*)
			  arg)))
	(find-time-gaps argx *time-gaps*)))

(defparameter *full-directory* nil)

(defun compare-directory-new (arg)
  (cond ((null arg)
		 (compare-directory *directory*))
		(t (xlogntf "compare-directory-new: unrecognized args, processing halted ~s" arg))))

(defun compare-directory ( &optional (dir "/home/data6/webcams/pendroy/scancam/pendroy/2020/05/30/") ) 
  "This compares files from a leaf directory in the full image tree: e.g., for daily saved images at ...pendroy/2020/05/30, we are looking at the *.jpg in 30"
  ;; TODO calcuate ratio of files considered same to number left and report.
  (let* ((full-dir-namestring (namestring (merge-pathnames dir)))
		 (log-fn (slashes-to-hyphens full-dir-namestring)))
	(with-open-log-file ((format nil "comp-dir~a" log-fn) :dates t :dir (list :relative (car (last (pathname-directory (uiop:ensure-directory-pathname dir)))))) ;; TODO: wow
	  (xlogntf "Danged log file is in fact open ~s" (the-log-file))
	  (let ((*trace-output* (the-log-file))
			(*error-output* (the-log-file)))
		(progn
		  (init-compare dir)
		  (xlogft "compare-directory ~a" (version-number-string "cd"))
		  (setf *full-directory* (sort (directory (concatenate 'string full-dir-namestring "/*.jpg")) 
									   'string<  :key #'(lambda (s)
														  (file-namestring s))))
		  (let* ((full *full-directory*)
				 (top (car full)))
			(xlogntf "cd: we have ~a images to check" (length full))
			(if (and *same-threshold* (not (consp *same-threshold*)) (zerop *same-threshold*))
				(xlogntf "cd: no threshold, saving time")
				(dolist  (ni  (cdr full))
				  (handler-case
					  (compare-images top ni)
					(error (e)
					  (incf *errors-encountered*)
					  (move-file-to-delete top "broken-images")
					  (xlogntf "cd: on image ~a, skipping~%error: ~a" top e)))
				  
				  (setf top ni))))
		  (debugc 5 (xlogntf "delete: ~a" *delete-these-files*))
		  (delete-files-from-list *delete-these-files* )
		  (xlogntft "~a images viewed ~a images deleted ratio ~6,2,f" 
					*images-viewed* *similar-images-deleted* 
					(if (plusp *images-viewed*)
						(/ (* 100.0 *similar-images-deleted*) *images-viewed*)
						0.0)))))))

(defun compare-archive-directories (&optional (dir "/home/data6/webcams/pendroy/scancam/apgar-visitor/")  )
  (global-init (directory-namestring dir))
  (let ((summary nil))
	(with-open-log-file ("sameness-summary" :dates t :dir  dir :show-log-file-name nil)
	  (dolist (dx (collect-year dir))
		(push (compare-directory dx) summary))
	  (push (xlogntft "overall, viewed ~a deleted ~a files" *global-images-viewed* *global-images-deleted*) summary))
	(dolist (k (nreverse summary))
	  (xlogf k)))
  nil)

(defun find-time-gaps (&optional (dirs "/home/wgl/projects/webcams/pendroy/rescancam/Pendroy/") (gap (* 5 60)))
  (let ((last-time nil)
        (count 0)
        (largest 0)
		(pairs nil)
        (total 0))
	(format t "find-time-gaps: gap ~s dirs ~s~%" gap dirs)
	(dolist (dir dirs)
	  (dolist (el (directory  (concatenate 'string `,dir "*.jpg")))
		(let* ((pair (cons (file-namestring el) (file-write-date el))))
		  (push pair pairs))
		(setf pairs (sort pairs '< :key 'cdr))))
	(dolist (lx pairs)
	  (let ((this-time (cdr lx)))
		(if (not last-time) 
			(setf last-time this-time))
		(let* ((df (- this-time last-time))
               (dfmin (/ df 60.0)))
          (incf total)
          (when (or (> df gap))
			(incf count)
			(format t "~35a - ~19a ~,2f~a~%" ;;~a(~a)
					(car lx) 
					(format nil "~a(~a)" this-time df)
					dfmin
					(cond ((> df largest)
                           "!")
                          (t " ")))
			
			(setf last-time this-time))
		  (when (> df largest)
			(setf largest df)))))
    (format t "we had ~a out of range out of ~a, largest ~a~%" count total largest)))

;; TODO: here is a good reference site for web cameras:

;; http://www.bigskyfishing.com/Web-Cams/Web-Cams.htm#regional
;; one of the lincoln cameras, drilled down: http://www.linctel.net/~jaimej26/images/webcam1.jpg (deep link)
;; http://www.bigskyfishing.com/Web-Cams/Web-Cams.htm

;; TODO apgar works with minute
;; goat haunt works with minute
;; arizona works with minute
;; middlefork works with minute
;; park headquaaarters looks more like 2 mins
;; two medicine two minutes
;; lake mcdonald looks like two minutes
;; many glacier mostly two minutes
;; st marys one minute


;; mdtrwis@mt.gov https://app.mdt.mt.gov/atms/swagger-ui/index.html
;; api key is NWFlNTQ1MjUtNzdhNi00M2I2LThhZTUtODBjY2QzZGYxY2Ex
;; curl -X 'GET'  'https://app.mdt.mt.gov/atms/api/conditions/v1/current?apiKey=NWFlNTQ1MjUtNzdhNi00M2I2LThhZTUtODBjY2QzZGYxY2Ex' -H 'accept: application/json'
;; to get images
;; curl -X 'GET'  'https://app.mdt.mt.gov/atms/api/conditions/v1/current/images?apiKey=NWFlNTQ1MjUtNzdhNi00M2I2LThhZTUtODBjY2QzZGYxY2Ex'  -H 'accept: application/json'
