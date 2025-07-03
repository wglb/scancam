(in-package #:scancam)

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0)))

(defparameter *errors-encountered* 0)
(declaim (fixnum *errors-encountered*))

(defparameter *delete-or-not* t)
(defparameter *delete-threshold* nil)
(defparameter *delete-threshold-directory* nil)
(defparameter *delete-threshold-default* 37)



(define-flag *process*
  :default-value "scancam" 
  :selector "process"
  :type string
  :help "Which process"
  :documentation "Which sub-process to run")

(define-flag *time-gaps*
  :default-value 300
  :selector "time-gaps"
  :type integer
  :help "Calculate time gaps of images"
  :documentation "Gaps for find-time-gaps; remainder of command line arguments specify directories")

(define-flag *dbg-flag*
  :default-value 0
  :selector "debug"
  :type integer
  :help "Turn on debugging mode")

(define-flag *files*
  :default-value ""
  :selector "files"
  :type string
  :help "List of files to process")

(define-flag *directory*
  :default-value ""
  :selector "directory"
  :type string
  :help "Directory to process"
  :documentation "Which directory to do")

(define-flag *log-file-aux*
  :default-value ""
  :selector "log-file-aux"
  :type string
  :help "Alternate log file component"
  :documentation "If you want to have different log file")

(define-flag *help*
  :default-value nil
  :selector "help"
  :documentation "Show options"
  :type boolean)

(defparameter *command-line* nil)

(defparameter *directory-use* nil)

(defparameter *command-line-args* nil)
