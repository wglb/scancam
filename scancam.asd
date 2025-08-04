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
			   #:local-time
			   #:dex-get)
    
  :version "2.20.2"
  :components ((:file "scancam-package")
			   (:file "globals")
			   (:file "scancam-tools")
               (:file "image-compare")
			   (:file "end-of-day-processing")
			   (:file "process-logs")
			   (:file "detect-stars")
			   (:file "dispatch")
			   (:file "rwis-cameras")
			   (:file "scancam")))



