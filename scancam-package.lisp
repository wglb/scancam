;;;; package.lisp

;;;; package.lisp

(defpackage #:scancam
  (:use #:cl
		#:xlog
		#:move-files
        #:cl-base64
        #:flexi-streams 
		#:com.google.flag
        #:dex
		#:dex-get
		#:cl-html-parse
        #:dup-images-by-hash
#+nil        #:replace-all
        #:lock-and-trigger
		#:configuration-r
		#:tokens
		#:cl-gd
        #:quri
		#:com.google.flag
		#:iolib/sockets
		#:str
		#:osicat)
    
  (:shadowing-import-from :dex :get :delete)
  (:shadowing-import-from :cl-gd :create-image))
