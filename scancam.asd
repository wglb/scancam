;;;; scancam.asd

(asdf:defsystem #:scancam
  :serial t
  :description "scan some road cameras"
  :author "Ciex, Incorporated"
  :license "Copyright Ciex,Incorporated(c) 2014, All rights reserved"
  :depends-on (#:xlog
			   #:cl-html-parse
			   #:com.google.flag
               #:flexi-streams
               #:cl-base64
               #:quri
               #:dexador
			   #:cl-gd
			   #:iolib/sockets
			   #:osicat
			   #:com.google.flag
			   #:tokens
               #:dup-images-by-hash
			   #:move-files
			   #:configuration-r
			   #:str
               #:lock-and-trigger
			   #:dex-get)
    
  :version "2.16.3"
  :components ((:file "scancam-package")
			   (:file "scancam-tools")
               (:file "dispatch")
			   (:file "image-compare")
			   (:file "scancam")
			   (:file "process-logs")
			   (:file "detect-stars")))



