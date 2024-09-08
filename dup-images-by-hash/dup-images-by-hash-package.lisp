;;;; package.lisp

(defpackage #:dup-images-by-hash
  (:use #:cl
        #:ironclad
		#:replace-all
		#:move-files
        #:xlog)
  (:export #:remove-duplicates-by-hash
		   #:remove-log-duplicates-by-hash
		   #:move-file-to-delete
		   #:calc-collision-free-file-name
		   #:make-list-files))

