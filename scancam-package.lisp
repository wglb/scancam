;;;; package.lisp

;;;; package.lisp

(defpackage #:scancam
  (:use #:cl
		#:move-files
        #:cl-base64
        #:flexi-streams 
        #:xlog
		#:com.google.flag
        #:dex
		#:dex-get
		#:cl-html-parse
        #:dup-images-by-hash
        #:replace-all
        #:lock-and-trigger
		#:configuration-r
		#:tokens
		#:cl-gd
        #:quri
		#:com.google.flag
		#:iolib/sockets
		#:osicat)
    
  (:shadowing-import-from :dex :get :delete)
  (:shadowing-import-from :cl-gd :create-image))
