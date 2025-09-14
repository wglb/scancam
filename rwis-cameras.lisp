(in-package #:scancam)

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0)))

(defparameter  *collected-bits* nil)
(defparameter *master-index-page* nil)

(defparameter *parsed-homepage* nil)

(defparameter *home-page-parts* nil)

(defparameter *saved-home-page* nil)

(defparameter *pendroy-base* "http://rwis.mdt.mt.gov/scanweb/Camera.asp?Pageid=Camera&Units=English&Groupid=301000&Siteid=301001&Senid=&Wxid=3011&Mapid=&DisplayClass=Java&SenType=All&HEndDate=&Zoneid=&Mode=&Sprayerid=&Dvid=&CD=9%2F12%2F2013+7%3A46%3A34+PM")

(defparameter *sweetgrass-base*
  "http://rwis.mdt.mt.gov/scanweb/Camera.asp?Pageid=Camera&Units=English&Groupid=629000&Siteid=629001&Senid=&Wxid=62911&Mapid=&DisplayClass=Java&SenType=All&HEndDate=&Zoneid=&Mode=1&Sprayerid=&Dvid=&CD=6%2F29%2F2014+5%3A50%3A20+PM")

(defun analyze-menu-page (pg)
  (dolist (fel pg)
	(when (the-tag-p fel :html)
	  (dolist (fmx fel)
		(when (the-tag-p fmx :body)
		  (dolist (fxxx fmx)
			(declare (ignorable fxxx))
			3))))))

(defun calc-dir-from-tokens (tokes)
  "Specific to the way RWIS formats file names"
  (cond ((eq 10 (length tokes))
		 (list (first tokes) (rest tokes)))
		((eq 11 (length tokes))
		 (list (format nil "~a-~a" (first tokes) (second tokes))  (rest (rest tokes))))
		((eq 12 (length tokes))
		 (list (format nil "~a-~a-~a" (first tokes) (second tokes) (third tokes)) (rest  (rest (rest tokes)))))
		((eq 13 (length tokes))
		 (list (format nil "~a-~a-~a-~a" (first tokes) (second tokes) (third tokes) (fourth tokes)) (rest (rest (rest (rest tokes))))))
		(t
		 (error "unknown length of ~a for ~s" (length tokes) tokes)))
  
  #+nil(if (equal 9 (length tokes))
		   (list (first tokes) (rest tokes))
		   (if (equal 10 (length tokes))
			   (list (format nil "~a-~a" (first tokes) (second tokes))  (rest (rest tokes)))
			   (if (>= (length tokes) 11) 
				   (list (format nil "~a-~a-~a" (first tokes) (second tokes) (third tokes))  (rest (rest (rest tokes))))
				   (error "unknown length of ~a for ~s" (length tokes) tokes)))))

(defun pull-new-rwis-image (which)
  "Pull the new style rwis images; answer the directory. rest of elements are descriptive of camera."
  (let* ((the-url (first which))
		 (path  (uri-path (uri the-url)))
		 (tokes (tokens (file-namestring (uri-path (uri path)))
						#'(lambda (c)
							(and (char/= #\- c) (char/= #\. c)))
						0)
				#+nil (tokenize1 (file-namestring path) #\-))
		 (dirx (calc-dir-from-tokens tokes))
		 (pfn (calc-path dirx)))
	(ensure-directories-exist pfn)
	(get-config-rescan (first dirx) :average)
	(with-open-log-file ((format nil "~a-~a" (first dirx) "rwis") :dir `(:relative ,(first dirx)))
	  (pull-rwis (cons pfn (uri the-url))))
	;; dirx is of the form ("Pendroy" ("301001" "01" "7" "3" "2025" "8" "45" "11" "jpg"))
	(first dirx)))

;; (calc-dir-from-tokens (tokenize1 "Aberdeen-Hill-263004-00-3-26-2024-12-15-1.jpg" #\-))

(defun restore-collected-bits ()
  "Restore the parsed, distilled home page"
  (with-open-file (fi "collected.bits.lsp" :direction :input)
	(setf *collected-bits* (read fi))
	(length *collected-bits*)))

(defun save-collected-bits ()
  "Save the parsed, distilled home page"
  (with-open-file (fo "collected.bits.lsp" :direction :output :if-exists :supersede :if-does-not-exist :create)
	(let ((*print-pretty* t))
	  (write *collected-bits* :stream fo))))

(defun find-images-new-home-page (lsp)
  (setf *collected-bits* nil) 
  (let ((*print-pretty* nil)
		(cams nil))
	(log-version-number "finhp")
	(finhp lsp)
	(setf *collected-bits* (ashuffle *collected-bits*))
	(with-open-file (fo (format nil "~a-rwis-cameras.out" (dates-ymd :ymd)  )  :direction :output :if-exists :supersede :if-does-not-exist :create)
	  (dolist (cb *collected-bits*)
		(let ((image (pull-new-rwis-image cb)))
		  (bump-images-camera-count image 1)
		  (pushnew image cams :test 'equal)))
	  (mapc #'(lambda (i)
				(write-line i  fo))
			(sort cams 'string<)))) 
  (save-collected-bits))

(defun calc-path (fn-tokesa)
  "For the Montana RWIS cameras"
  (let* ((dir (first fn-tokesa))
		 (fn-tokes (second fn-tokesa))
		 (camera-id (first fn-tokes))
		 (extra (second fn-tokes))
		 (year (parse-integer (fifth fn-tokes)))
		 (month (parse-integer (third fn-tokes)))
		 (day (parse-integer (fourth  fn-tokes)))
		 (hour (parse-integer (sixth fn-tokes)))
		 (minute (parse-integer(seventh fn-tokes)))
		 (extra2 (tokenize1 (eighth fn-tokes) #\.))
		 (typ (car (last fn-tokes)))
		 (aux (first extra2))
		 (fmt (list
			   (cons "~a-"     camera-id)
			   (cons "~a-"     extra)
			   (cons "~4,'0D-" year )
			   (cons "~2,'0D-" month)
			   (cons "~2,'0D-" day)
			   (cons "~2,'0D-" hour)
			   (cons "~2,'0D-" minute)
			   (cons "~2,'0D" aux)))
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


#+nil (defun try-pendroy-new-raw ()
  (cond ((get-rwis-home-page "current-new") 
		 (find-images-new-home-page *saved-home-page*)
		 (with-open-file (fod "live-directories.lsp"  ;; This is redundant with 'images-by-camera, but not as up to date
							  :direction :output :if-exists :supersede :if-does-not-exist :create)
		   (write (all-image-directories) :stream fod)))
		(t (xlogntft "tpn: home page fetch failure"))))

(defun try-pendroy-new ()
  (if (get-rwis-home-page "current-new") 
	  (handler-case
		  (progn
			(find-images-new-home-page *saved-home-page*)
			#+nil (with-open-file (fod "live-directories.lsp"  ;; This is redundant with 'images-by-camera, but not as up to date
								 :direction :output :if-exists :supersede :if-does-not-exist :create)
			  (write (all-image-directories) :stream fod)))
		(error (e)
		  (progn
			(xlogntft "tpn: error in new rwis home page processing ~s:" e)
			(break "tpn: botch"))))
	  (xlogntft "tpn: home page fetch failure")))

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
			   (setf *saved-home-page* (write-sexp-lsp ppbody (format nil "rwis-cameras-new-~a.lsp" base)))
			   (write-sexp-lsp ppbody (format nil "rwis-cameras-new-~a.lsp" (dates-ymd :ym))))))) 
  
  (length *saved-home-page*))

(defun restore-rwis-home-page (&optional (base ""))
  (with-open-file (fi (format nil "rwis-cameras-new-~a.lsp" base))
	(setf *saved-home-page* (read fi)))
  (length *saved-home-page*))


