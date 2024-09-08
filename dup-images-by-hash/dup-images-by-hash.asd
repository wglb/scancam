;;;; dup-images-by-hash.asd

(asdf:defsystem #:dup-images-by-hash
  :description "Describe dup-images-by-hash here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "1.0.5" 
  :serial t
  :depends-on (#:ironclad
			   #:replace-all
			   #:move-files
               #:xlog)
  :version "1.1.3"  
  :components ((:file "dup-images-by-hash-package")
               (:file "dup-images-by-hash")))


