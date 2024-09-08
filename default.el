(message "begin of pondera  scancam default")

(setq inferior-lisp-program "sbcl --dynamic-space-size 8192 --control-stack-size 4")

(defun fileaway ()
  (interactive)
  ;;(message (format "my my aching files %s" (dired-get-marked-files)))
  (let ((s (buffer-name)))
	(if (string-match "\\([jJtl][pPis][gGfp]$\\)" s)
		(message "This is not a directory, but a jpg bleeping file!"))
	(progn
	  (dired-run-shell-command
	   (dired-shell-stuff-it "file-away-override" (dired-get-marked-files) nil nil ))
	  (message "about to redisplay")
	  (dired-unmark-all-marks)
	  (revert-buffer))))


;;(global-set-key [f5] 'fileaway)

(global-set-key [f5] 'next-pic-no-mark)

(defun poser ()
  (interactive)
  (dired-run-shell-command (format "%s poser/" (dired-shell-stuff-it "cp -v" (dired-get-marked-files) nil nil))))

(global-set-key [f6] 'poser)

(defun astronomy ()
  (interactive)
  (dired-run-shell-command (format "%s astronomy/" (dired-shell-stuff-it "cp -v" (dired-get-marked-files) nil nil))))

(global-set-key [S-f6] 'astronomy)

(defun astronomy-delete ()
  (interactive)
  (dired-run-shell-command (format "%s astronomy/" (dired-shell-stuff-it "mv -v" (dired-get-marked-files) nil nil)))
  (revert-buffer))

(global-set-key [S-f9] 'astronomy-delete)

(defun astronomy-parent ()
  (interactive)
  (dired-run-shell-command (format "%s ../../astronomy/" (dired-shell-stuff-it "cp -v" (dired-get-marked-files) nil nil))))


(defun post (fn)
  (interactive (list 
				(read-directory-name "Which destination: " "./"
									 "~/Downloads/misc-pictures/")))
  (message "You got %s" fn)
  (dired-run-shell-command (format "%s %s/" (dired-shell-stuff-it "cp -v" (dired-get-marked-files) nil nil) fn)))

(global-set-key [f8] 'post)

(defun known-good ()
  (interactive)
  (dired-run-shell-command (format "%s known-good/" (dired-shell-stuff-it "mv -v" (dired-get-marked-files) nil nil)))
  (revert-buffer))
  
(global-set-key [S-f3] 'known-good)

(defun next-pic ()
  (interactive)
  (kill-buffer)
  (dired-mark nil)
  (dired-find-file))

(defun next-pic-s ()
  (interactive)
  (let ((s (buffer-name)))
	(if (string-match "\\([jJt][pPi][gGf]$\\)" s) ;; (string= "jpg" (match-string 0 s))
		(progn
		 (kill-buffer)
		 (dired-mark nil)
		 (dired-find-file))
	  (message "End of jpg, dude"))))


;;(let ((radio "video.jpg"))
;;  (string= "jpg" (match-string 0 radio)))

;;(kill-buffer)
;;  (dired-mark nil)
;;(nonincremental-search-forward ".jpg")
;;(dired-find-file)

;;(global-set-key [f4] 'next-pic-s)

(defun next-pic-x ()
  (interactive)
  (let ((s (buffer-name)))
	(if (string-match "\\([jJt][pPi][gGf]$\\)" s) ;; (string= "jpg" (match-string 0 s))
		(progn
		 (kill-buffer)
		 (dired-mark nil)
		 (nonincremental-search-forward "jpg")
		 (dired-find-file))
	  (message "End of jpg, dude"))))

(global-set-key [f4] 'next-pic-x)



(setq large-file-warning-threshold nil)

(defun next-pic-no-mark ()
  (interactive)
  (let ((s (buffer-name)))
	(string-match "\\(jpg$\\)" s)
	(if (string-match "\\([jJ][pP][gG]$\\)" s) ;;(string-match "\\(jpg$\\)" s) ;; (string= "jpg"(match-string 0 s))
		(progn
		 (kill-buffer)
		 (dired-unmark nil)
		 (dired-find-file))
	  (message "End of jpg, dude"))))
  
(global-set-key [S-f4] 'next-pic-no-mark)

;;(color-theme-zenburn)
;;(cond ((file-exists-p (concat defaul
(if (string= "lake" system-name-split)
	(color-theme-zenburn)	
	(load-theme 'modus-vivendi))
(if (string= "lake" system-name-split)
	(message "we seem to be lake"))
(message system-name-split)
(message "end of pondera scancam default")
