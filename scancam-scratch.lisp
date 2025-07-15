(get-config-rescan (make-pathname :directory '(:relative "Pendroy"))  :average)


(pathname-directory #P"Pendroy/2024/05//")


(pathname-directory (namestring #P"Pendroy/2024/05//"))

(merge-pathnames (make-pathname :directory '(:relative (pathname-directory "Clancy/delete-similar"))) (make-pathname :name "rescancam" :type "lsp"))


(merge-pathnames (directory-namestring "wh-marina"))

(init-compare "wh-marina")

(namestring (merge-pathnames "wh-marina"))

(pathname-directory (uiop:ensure-directory-pathname "wh-marina"))

(full-dir-namestring (namestring (merge-pathnames "wh-marina")))


(namestring (uiop:ensure-directory-pathname "/home/data6/webcams/pendroy/scancam/wh-marina/2024/12/29"))
(namestring (uiop:ensure-directory-pathname #P"/home/data6/webcams/pendroy/scancam/wh-marina/2024/12/29"))


(full-dir-namestring (namestring (uiop:ensure-directory-pathname "Gardiner")))
(pathname-directory (uiop:ensure-directory-pathname "Gardiner"))
(namestring )
(directory (concatenate 'string (namestring (uiop:ensure-directory-pathname "Gardiner")) "*.jpg"))

(with-open-log-file ("radio" :dates t :dir (pathname-directory (uiop:ensure-directory-pathname "Gardiner")))
  (xlogntf "radio")
  nil)

(init-compare "Gardiner")

(make-pathname :directory '(:RELATIVE "Clancy") :name "lo" :type "rdo")

(make-pathname :directory (pathname-directory "/home/data6/webcams/pendroy/scancam/Clancy/2025/01/18/") :name "lo" :type "rdo")

(make-pathname :directory (pathname-directory "/home/data6/webcams/pendroy/scancam/Clancy/2025/01/18/") :name "lo" :type "rdo")


(pathname-directory "/home/data6/webcams/pendroy/scancam/Clancy/2025/01/18/")


(detect-stars-in-file "/home/data6/webcams/pendroy/scancam/Pendroy/" "/home/data6/webcams/pendroy/scancam/Pendroy/star/301001-01-2025-01-07-17-45-4.jpg")

(detect-stars-in-file-brightness "Pendroy/star" "301001-01-2025-01-07-17-45-4.jpg" 1)


(let* ((ray (make-array '(4 5) :initial-element 0))
	   (dims (array-dimensions ray))
	   (count 0))
  
  (dotimes (i (first dims))
	(dotimes (j (second dims))
	  (setf (aref ray i j) (incf count))))
  ray)


(defun fix-threeforks (url)
  (calc-dir-from-tokens 
   (tokens (file-namestring (uri-path (uri url)))
		   #'(lambda (c)
			   (and (char/= #\- c) (char/= #\. c)))
		   0)))


 (PULL-NEW-RWIS-IMAGE ("https://mdt.mt.gov/other/WebAppData/External/RRS/RWIS/Judith-Gap-268001-01-2-28-2025-11-45-17.jpg" "West Elev 4677 - 02/28/2025 11:45 AM" "268011"))

(defun one-hit (cb)
  (let ((tfns (tokens (file-namestring (uri-path (uri (first cb))))
		   #'(lambda (c)
			   (and (char/= #\- c) (char/= #\. c)))
		   0)
				  #+nil (tokenize1 (file-namestring (uri-path (uri (first cb)))) #\-)))
		(xlogntf "~a: ~s, ~%    path ~s" (length tfns) tfns (calc-path-obs (calc-dir-from-tokens tfns)))))

(defun hit-coll  (coll)
  (let ((*print-pretty* nil))
	(dolist (cb coll)
	  (one-hit cb))))
(defun one-hit-3forks ()
  (one-hit '("https://mdt.mt.gov/other/WebAppData/External/RRS/RWIS/Three-Forks-I-90-564012-00-2-28-2025-14-0-1.jpg" "East Bound Elev 4260 - 02/28/2025 02:00 PM" "564120")))


Ready to pull new rwis image dirx ("Malta-South"
                                   ("269000" "02" "2" "28"
                                    "2025" "12" "45" "5" "jpg")) path #P"Malta-South/269000-02-2025-02-28-12-45-5"
   [Condition of type SIMPLE-CONDITION]



("https://mdt.mt.gov/other/WebAppData/External/RRS/RWIS/Three-Forks-I-90-564012-00-2-28-2025-12-41-59.jpg" "East Bound Elev 4260 - 02/28/2025 12:41 PM" "564120")

base (#P"Pendroy/301001-03-2025-02-27-23-00-6.jpg" . #<URI-HTTPS https://mdt.mt.gov/other/WebAppData/External/RRS/RWIS/Pendroy-301001-03-2-27-2025-23-0-6.jpg>)
#P"Pendroy/301001-03-2025-02-27-23-00-6.jpg" url #<URI-HTTPS https://mdt.mt.gov/other/WebAppData/External/RRS/RWIS/Pendroy-301001-03-2-27-2025-23-0-6.jpg>
wif: #P"/home/data6/webcams/pendroy/scancam/Pendroy/301001-03-2025-02-27-23-00-6.jpg"

---

base (#P"Three-Forks-I/90-564012-0028-01-02-2025-13-30" . #<URI-HTTPS https://mdt.mt.gov/other/WebAppData/External/RRS/RWIS/Three-Forks-I-90-564012-01-2-28-2025-13-30-1.jpg>)
#P"Three-Forks-I/90-564012-0028-01-02-2025-13-30" url #<URI-HTTPS https://mdt.mt.gov/other/WebAppData/External/RRS/RWIS/Three-Forks-I-90-564012-01-2-28-2025-13-30-1.jpg>
content type "image/jpeg"
wif: #P"/home/data6/webcams/pendroy/scancam/Three-Forks-I/90-564012-0028-01-02-2025-13-30"
2025-02-28 14:32:36.502248 xlog: end of log-file Three-Forks-I/2025-02-28_Three-Forks-I-rwis.log



(full-dir-namestring (namestring (merge-pathnames "/home/data6/webcams/pendroy/scancam/Three-Forks-I-90/")))


(make-pathname (uiop:ensure-directory-pathname "/home/data6/webcams/pendroy/scancam/wh-marina")  (make-pathname :directory '(:relative "logs")))
(make-pathname  :directory "/home/data6/webcams/pendroy/scancam/wh-marina"   (make-pathname :directory '(:relative "logs")))

(make-pathname :directory (append (pathname-directory "/home/data6/webcams/pendroy/scancam/wh-marina") (list "logs")))




(- (unix-to-timestamp (sb-posix:stat-mtime (sb-posix:stat "/home/data6/webcams/pendroy/scancam/Pendroy/301001-00-2025-06-08-22-00-1_1.jpg")))
   (unix-to-timestamp (sb-posix:stat-mtime (sb-posix:stat "/home/data6/webcams/pendroy/scancam/Pendroy/301001-00-2025-06-08-22-00-1_2.jpg"))))

(timestamp-difference (unix-to-timestamp (sb-posix:stat-mtime (sb-posix:stat "/home/data6/webcams/pendroy/scancam/Pendroy/301001-00-2025-06-08-22-00-1_2.jpg")))
					  (unix-to-timestamp (sb-posix:stat-mtime (sb-posix:stat "/home/data6/webcams/pendroy/scancam/Pendroy/301001-00-2025-06-08-22-00-1_1.jpg"))))


(- (sb-posix:stat-mtime (sb-posix:stat "/home/data6/webcams/pendroy/scancam/Pendroy/301001-00-2025-06-08-22-00-1_2.jpg"))
   (sb-posix:stat-mtime (sb-posix:stat "/home/data6/webcams/pendroy/scancam/Pendroy/301001-00-2025-06-08-22-00-1_1.jpg")))
