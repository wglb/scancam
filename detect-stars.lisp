(in-package #:scancam)

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0)))

(defun brightness (img x y)
  (let ((pix (get-pixel x y :image img)))
	(floor (/ (+ (ldb (byte 8 16) pix) (ldb (byte 8 8) pix) (ldb (byte 8 0) pix)) 3.0))))

(defun swell-rectangle (rect delt)
  (let* ((ans (list (max (- (first rect ) delt) 0)
					(max (- (second rect ) delt) 0)
					(max (+ (third rect ) delt) 0)
					(max (+ (fourth rect ) delt) 0))))
	(debugc 5 (xlogntf "swell input ~s output ~s"  rect ans))
	ans))

(defun center-rect (rect)
  (list (+ (first rect)  (/ (- (third rect) (first rect)) 2))
		(+ (second rect) (/ (- (fourth rect) (second rect)) 2))))

(defun size-rect (rect)
  (debugc 5 (xlogntf "sr: rect ~a" rect))
  (* (- (first rect) (third rect))
	 (- (second rect) (fourth rect))))

(defun distance (x1 y1 x2 y2)
  (sqrt (+ (* (- x1 x2) (- x1 x2))
		   (* (- y1 y2 ) (- y1 y2 )))))

(defun distance2 (pair1 pair2)
  (distance (first pair1) (second pair1) (first pair2) (second pair2)))

(defun compare-rect (a b delta)
  "a and b are coordinates of rectangles. True if close"
  (let ((dist (distance2 (center-rect a) (center-rect b))))
	(list (< dist delta)
			dist)))

(defun remove-overlap-by-distance (rects delt)
  "Remove overlap amongst a list of rectangles"
  (let* ((sorted-rex rects  #+nil (sort rects '< :key 'car))
		 (delta2 (distance 0 0 (+ delt #+nil  delt) (+ delt #+nil delt)))
		 (*print-pretty* nil)
		 (far-rect nil))
	(debugc 5 (xlogntf "robd: delts: ~a starting rects is ~%~s" 
					   delt
					   (mapcar #'(lambda (lx)
								   lx)
							   rects)))	
	(if rects
		(push (first rects) far-rect)
		(xlogntf "robd: Oops--aingotno rects ~a for delt ~a" rects delt))
	(debugc 5 (xlogntf "robd: delta2 is ~a starting rects ~s ~s" delta2 (length sorted-rex ) sorted-rex))
	(debugc 5 (xlogntf "robd: have ~a rects, delta ~a delta2 ~a" (length rects) delt delta2))
	(dolist (rx rects)
	  (let ((match t))
		(dolist (whx far-rect)
		  (let ((dist (compare-rect rx whx delta2)))
			(debugc 5 (xlogntf "robd: fx a ~a b ~a ~a"  rx whx dist))
			(if (first dist) ;; if it is close to one that is laready there, don't push
				(setf match nil))))
		(if match
			(pushnew rx far-rect))))
	
	(debugc 5 (xlogntf
			   "robd: finishing result is ~s"
			   (mapcar #'(lambda (lx)
						   lx)
					   far-rect)))
	far-rect))

(defparameter *draw-grid* nil)

(defun find-clusters (candidates delt)
  "answer a list of sublists (with size as first element) of clusters within a delta of each other"
  (let ((list-of-current-clusters nil)
		(list-of-clusters nil)
		(top-cluster (first candidates))
		(delta2 (distance 0 0 (+ delt delt) (+ delt delt)))
		(*print-pretty* nil))
	
	(dolist (ix candidates)
	  (let ((dist (distance (first top-cluster) (second top-cluster) (first ix) (second ix))))
		(cond ((< dist delta2)
			   (debugc 5 (xlogntf "fc: ix within tolerance ~a dist ~a" ix dist))
			   (push ix list-of-current-clusters))
			  
			  (t
			   (debugc 5 (xlogntf "fc: new cluster with ~a dist ~a" ix dist))
			   (if list-of-current-clusters
				   (push (list (length list-of-current-clusters) list-of-current-clusters) list-of-clusters))
			   (setf list-of-current-clusters nil)
			   (setf top-cluster ix)))))
	(when list-of-current-clusters
	  (push (list (length list-of-current-clusters) list-of-current-clusters) list-of-clusters))
	
	(debugc 5 (xlogntf "fc: we have ~a clusters " (length list-of-clusters)))
	(debugc 5 (dolist (cx list-of-clusters)
	  (xlogntf  "~a " cx)))
	list-of-clusters))

(defun find-bright-spots-int (fn img width height delt brightness-diff-thresh overlap-thresh)
  "Look for gradients in brightness at delt and 2*delt away from center than exceeds brightness"
  (let ((possibles (list (list fn :width width :height height :astronomy-delt delt :astronomy-thresh brightness-diff-thresh :overlap-threshold overlap-thresh)))
		(hits nil)
		(delta2 (* 3 delt))
		(oldmax 0))
	(dotimes (x (- width delta2))
	  (let ((x-left (- x delt))
			(x-left2 (- x delta2))
			(x-right (+ x delt))
			(x-right2 (+ x delta2)))
		(when (and (plusp x-left) (< x-right2 width) t)
		  (dotimes (y (- height delta2))
			(let ((y-up (- y delt))
				  (y-up2 (- y delta2))
				  (y-down (+ y delt))
				  (y-down2 (+ y delta2)))
			  (when (and (plusp y-up2) (< y-down2 height) t)
				(let* ((bright-left (brightness img x-left y))
					   (bright-leftw (brightness img x-left2 y))
					   (bright-right (brightness img x-right y))
					   (bright-rightw (brightness img x-right2 y))
					   (bright-up (brightness img x y-up))
					   (bright-upw (brightness img x y-up2))
					   (bright-down (brightness img x y-down))
					   (bright-downw (brightness img x y-down2))
					   (bright-center (brightness img x y))
					   (max-brightness-diff (max (abs (- bright-left bright-right)) (abs (- bright-up bright-down))))
					   (max-brightness-diff-wide (max (- bright-leftw bright-rightw) (abs (- bright-upw bright-downw)))))
				  (debugc 5 (xlogntf "fbsi: max-brightness-diff: ~a  ~a at ~a ~a"
									 max-brightness-diff 
									 (list (abs (- bright-left bright-right) ) (abs (- bright-up bright-down)) bright-center) x y))
				  (when (> max-brightness-diff oldmax)
					(debugc 5 (xlogntf "fbsi: max-brightness-diff ~a greater than oldmax ~a at ~a ~a" max-brightness-diff oldmax x y))
					(setf oldmax max-brightness-diff))
				  (when (> max-brightness-diff brightness-diff-thresh)
					(debugc 5 (xlogntf "fbsi: max-brightness-diff=~a max-brightness-diff-wide=~a ans over brightness-diff-thresh=~a" 
									   max-brightness-diff max-brightness-diff-wide brightness-diff-thresh))
					(when  (<= max-brightness-diff-wide brightness-diff-thresh) ;; TODO -- why less?  Ah, because this is the edges.
					  (push (list x y max-brightness-diff) hits)
					  (debugc 5 (xlogntf "fbsi: pushing ~s" (list x y max-brightness-diff)))))
				  bright-center)))))))
	(append possibles (list (reverse hits)))))

(defun draw-grid-maybe (width height new-image)
  (when *draw-grid*
	(let ((xincr (floor (/ width 10)))
		  (yincr (floor (/ height 10))))
	  (xlogntf "dgm: xincr ~a yincr ~a" xincr yincr)
	  (dotimes (xx 10)
		(draw-line (* xx xincr ) 0 (* xx xincr) height :color 255 :image new-image))
	  (dotimes (yy 10)
		(draw-line 0 (* yy yincr) width (*  yy yincr) :color 255 :image new-image)))))

(defun find-bright-spots (fn delt brightness-diff-thresh cluster-limit overlap-thresh) ;; use cluster limit TODO
  (declare (ignorable cluster-limit))
  (with-image-from-file (img fn :jpg)
	(multiple-value-bind (width height)
		(image-size img)
	  (let* ((poss (find-bright-spots-int fn img width height delt brightness-diff-thresh overlap-thresh))
			 (new-directory (append (pathname-directory fn) (list "marked-images"))))
		(with-image (new-image width height t)
		  (dotimes (i +max-colors+)
			(allocate-color i i i :image new-image))
		  (debugc 5 (xlogntf "fbs: poss from int ~%~a" poss))
		  (copy-image img new-image 0 0 0 0 width height) ;; ?? Dupe?
		  (copy-palette img new-image)
		  (draw-grid-maybe width height new-image)
		  (let ((list-o-clusters (find-clusters (cadr poss) delt))
				(white  (allocate-color 255 255 255 :image new-image) #+nil (find-color 0 255 0 :image new-image)))
			(let ((sublist
					(if (< (length list-o-clusters) cluster-limit)
						list-o-clusters
						(subseq list-o-clusters 0 cluster-limit)))
				  (rectangle-list nil)
				  (rectangle-and-orig nil))
			  ;; Now goup points into clusters. Note that width can vary.
			  (dolist (dotts sublist)
				(let* ((dots (second dotts))
					   (firsts (mapcar 'car dots))
					   (seconds (mapcar 'second dots))
					   (maxx (reduce 'max firsts))
					   (minx (reduce 'min firsts))
					   (maxy (reduce 'max seconds))
					   (miny (reduce 'min seconds))
					   (result (list minx miny maxx maxy)))
				  (push (list dotts result) rectangle-and-orig)
				  (push result rectangle-list)
				  (debugc 5 (xlogntf "fbs: maxx ~a minx ~a maxy ~a miny ~a" maxx minx maxy miny))))
			  
			  (let* ((reduced (remove-overlap-by-distance rectangle-list overlap-thresh)))
				(dolist (rx reduced)
				  (let ((swollen (swell-rectangle rx delt)))
					(debugc 5 (xlogntf "fbs: swollen from ~a size ~a is ~a size ~a" rx (size-rect rx) swollen (size-rect swollen)))
					(draw-rectangle swollen  :color white #+nil (+ 255 (* 255 255) (* 255 255 255)) :image new-image)))
				(let ((new-file-name 
						(make-pathname :name (format nil "~a_~a-~a-~a" (pathname-name fn)  delt brightness-diff-thresh "circle")
									   :directory new-directory :type "jpg")))
				  (when (plusp (length reduced))
					(ensure-directories-exist new-file-name)
					(xlogntf "fbs: new file name is ~a" new-file-name)
					(write-image-to-file new-file-name :image  new-image :if-exists :supersede)
					(incf *astronomy-images-found*)))
				(debugc 5 (let ((rereduced (remove-overlap-by-distance reduced overlap-thresh)))
							(xlogntf "----------------------------------------~%reduced has ~a elements, ~%rereduced has ~a elements~%----------------------------------------~%" 
									 reduced rereduced)))
				(if (or list-o-clusters rectangle-and-orig)
					(list *version-number* (append (list poss) (list list-o-clusters)) rectangle-and-orig)
					(progn
					  (xlogntf "fbs: No images ~s ~s" list-o-clusters rectangle-and-orig)
					  nil))))))))))

(defun do-find-bright-spots (file-name delt brightness-diff-thresh cluster-limit overlap-thresh)
  "Find the bright spots, and unconditionally write the parameters discovered."
  (xlogntf "ddfbs: -> file ~a delt ~a brightness-diff-threshold ~a"
		   file-name delt brightness-diff-thresh) ;; TODO file name generation to single function
  (let ((fn (format nil "~a/marked-images/~a_~a-~a.lsp" 
					(directory-namestring file-name) (pathname-name file-name) delt brightness-diff-thresh)))
	(xlogntf "dfbs: writing paramters to ~a" fn)
	(ensure-directories-exist fn)
	(let ((stuffa (find-bright-spots file-name delt brightness-diff-thresh cluster-limit overlap-thresh))
			(*print-right-margin* 128))
		(if stuffa
			(with-open-file (fo fn :direction :output :if-exists :supersede :if-does-not-exist :create)
			  (write stuffa :stream fo))
			(xlogntf "dfbs: No images")))
	(xlogntf "ddfbs: <- done")))

(defun detect-stars-in-file (dir fn)
  (let ((delt (get-config-rescan dir :astronomy-delt)))
	(when (plusp delt)
	  (let ((brightness-diff-thresh (get-config-rescan dir :astronomy-thresh))
			(cluster-limit (get-config-rescan dir :cluster-limit))
			(overlap-thresh (get-config-rescan dir :overlap-threshold)))
		(xlogntf "dsif: delt ~a brightness-diff-thresh ~a cluster-limit ~a overlap-thresh ~a"
				 delt brightness-diff-thresh cluster-limit overlap-thresh)
		(do-find-bright-spots fn delt brightness-diff-thresh cluster-limit overlap-thresh)))))

(defun detect-stars-in-file-brightness (dir fn count)
  "Try find brightness with range of brightness"
  (with-open-log-file ("brightness-scan" :dir dir )
	  (xlogntf "dsifb: directory ~a file ~a count ~a" dir fn count)
	(let ((delt (get-config-rescan dir :astronomy-delt)))
	  (when (plusp delt)
		(let ((brightness-diff-thresh (get-config-rescan dir :astronomy-thresh))
			  (cluster-limit (get-config-rescan dir :cluster-limit))
			  (overlap-thresh (get-config-rescan dir :overlap-threshold)))
		  (xlogntf "dsif: delt ~a brightness-diff-thresh ~a cluster-limit ~a overlap-thresh ~a"
				   delt brightness-diff-thresh cluster-limit overlap-thresh)
		  (dotimes (bx count)
			(let ((hcount (floor (/ count 2))))
			  (do-find-bright-spots (format nil "~a/~a" dir fn) delt
				(+ (- brightness-diff-thresh hcount) bx)
				cluster-limit overlap-thresh))))))))

(defun detect-stars-in-file-test (longfn)
  (setf *draw-grid* nil)
  (detect-stars-in-file (directory-namestring longfn) longfn) )

(defun detect-stars (dir)
  (setf *astronomy-images-found* 0)
  (with-open-log-file  ("brightness" :dir dir :dates t :append-or-replace :append)
	(let ((delt (get-config-rescan dir :astronomy-delt))
		  (brightness-diff-thresh (get-config-rescan dir :astronomy-thresh))
		  (cluster-limit (get-config-rescan dir :cluster-limit))
		  (overlap-thresh (get-config-rescan dir :overlap-threshold)))
	  (xlogntf "ds: delt ~a brightness-diff-thresh ~a cluster-limit ~a overlap-thresh ~a" delt brightness-diff-thresh cluster-limit overlap-thresh)
	  (dolist (lx (directory (format nil "~a/*.jpg" dir)))
		(do-find-bright-spots lx delt brightness-diff-thresh cluster-limit overlap-thresh))
	  (xlogntft "ds: ~a star images tagged " *astronomy-images-found*))))

	  ;; Exclusions these hits: /home/data6/webcams/pendroy/scancam/Aberdeen-Hill-I-90-MP-552_3/delete-darkness/marked-images/exclude/
	  ;; /home/data6/webcams/pendroy/scancam/Rock-Springs-MT59-MP36_5/delete-darkness/marked-images/Vid-000563002-00-00-2020-08-31-09-39_8-150-circle.jpg
	  ;; /home/data6/webcams/pendroy/scancam/Rock-Springs-MT59-MP36_5/delete-darkness/marked-images/exclusion/
										;/home/data6/webcams/pendroy/scancam/Geyser-US87-MP-23/delete-darkness/marked-images/exclusions/
	  ;; /home/data6/webcams/pendroy/scancam/Pendroy-US89-MP-62_6/delete-darkness/marked-images/exclusions
	  ;; /home/data6/webcams/pendroy/scancam/Hysham-Hills-I-94-MP-60_0/comet/marked-images/exclude/
	  ;; /home/data6/webcams/pendroy/scancam/Judith-Gap-US-191-MP-18_6/comet/marked-images/exclude/
	  ;; /home/data6/webcams/pendroy/scancam/Malta-South-US191-MP-122_5/comet/marked-images/exclude
	  ;; /home/data6/webcams/pendroy/scancam/Geyser-US87-MP-23/comet/marked-images/exclusion/
	  ;; /home/data6/webcams/pendroy/scancam/Ekalaka-S323-MP-46_8/comet/marked-images/exclude/
	  ;; /home/data6/webcams/pendroy/scancam/East-Livingston-I-90-MP-337_7/delete-darkness/marked-images/exclude/
	  ;; /home/data6/webcams/pendroy/scancam/Ekalaka-S323-MP-46_8/delete-darkness/marked-images/exclude/


(defun detect-stars-new (arg)
  "Detect stars in directory specified by *directory*, ignoring the arg"
  (cond  (arg
		  (xlogntf "detect-stars-new: Unexpected args. Processing halted ~s" arg))
		 (t
		    (xlogntft "We are at detect-stars-new with arg of ~s" *directory*)
			(detect-stars *directory*))))

;;  Exclusions: how to determine if point  is outside rectangle: https://stackoverflow.com/questions/2752725/finding-whether-a-point-lies-inside-a-rectangle-or-not


;; --------------------------------------------------------------------------------
;; 	(xlogntf "dfbs: writing paramters to ~a" fn) to see where these are written. Part of check for darkness.
;; Now, find the lsp files corresponding to the exclusions and combine them to see if they will be useful in preventing detection.


(defun locate-points-file (frm)
  (let ((pts (uiop:pathname-parent-directory-pathname frm)))
	(xlogntf "points: ~s" pts)
	pts))

(defun locate-mark-files-from-lst (line)
  (let ((dirs nil)
		(jpg-files (directory (concatenate 'string line "*.jpg")))
		(jpg-basenames nil))
	(dolist (fx jpg-files)a
	  (xlogntf "we have jpg files of ~s" jpg-files)
	  (xlogntf "calcuating base name of ~s with ~s is ~s" 
			   (file-namestring fx) 
			   (position #\_ (file-namestring fx))
			   (subseq (file-namestring fx)
					   0 (position #\_ (file-namestring fx))))
	  (push 
	   (subseq (file-namestring fx)
			   0 (position #\_ (file-namestring fx)))
	   jpg-basenames )
	  (pushnew (locate-points-file fx) dirs))
	(xlogntf "check dirs ~a" dirs)
	(xlogntf "jpg file basenames ~s" jpg-basenames)
	(dolist (dx dirs)
	  (xlogntf "as a list ~s" (pathname-directory dx))
	  (xlogntf "as a list ~s" (butlast (pathname-directory dx)))
	  (xlogntf "bright as a list ~s" (append (butlast (pathname-directory dx)) (list "bright")))
	  (dolist (sdx (list  (cons "marked-images" "*.jpg")  (cons "bright" "*.lsp")))
		(let* ((appendage (append (pathname-directory dx) (list (car `,sdx))))
			   (bright-pathname (make-pathname :directory appendage)))
		  (xlogntf "pathname of ~a as a list ~s" sdx bright-pathname)
		  (xlogntf "trying ~s" (concatenate 'string (namestring bright-pathname) (cdr `,sdx)))
		  (let ((bright-filenames  (directory (concatenate 'string (namestring bright-pathname) (cdr `,sdx))))
				(match-count 0))
			(with-open-file (fo "needed-jpg.lst" :direction :output :if-exists :supersede :if-does-not-exist :create)
			  (dolist (bx bright-filenames)
				(dolist (nx jpg-basenames)
				  (cond ((search nx (file-namestring bx))
						 (incf match-count)
						 (xlogntf "searching for corresponding: found ~s to ~s gets ~s, ~%   desired file is ~s"
								  nx (file-namestring bx)
								  (search nx (file-namestring bx))
								  bx))
						(t (format fo "~a~%" nx)))))
			  (xlogntft "we matched ~a and have ~a ~a  filenames" match-count sdx (length bright-filenames)))))))))

(defun test-locate-mark-files-from-lst ()
  (position #\_  (file-namestring #P"/home/data6/webcams/pendroy/scancam/Boulder-Hill-I-15-MP-170_9/delete-darkness/exclude/Vid-000267003-00-00-2020-08-15-04-41_8-150-circle.jpg"))
  (subseq (file-namestring #P"/home/data6/webcams/pendroy/scancam/Boulder-Hill-I-15-MP-170_9/delete-darkness/exclude/Vid-000267003-00-00-2020-08-15-04-41_8-150-circle.jpg") 0 36)
  (with-open-log-file ("locate-mark-files-from-lst" :append-or-replace :supersede)
	(locate-mark-files-from-lst  "./Boulder-Hill-I-15-MP-170_9/delete-darkness/exclude/" )))
