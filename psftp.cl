(in-package :cl-user)

(eval-when (compile load eval)
  (require :ftp)
  (require :util-string))

(defpackage :net.sftp.client
  (:use #:excl #:common-lisp))

(in-package :net.sftp.client)

(defun wait-for-response (stream)
  (let ((line)
	(char)
	(results ()))
    (macrolet ((next-char ()
		 `(prog1 (setq char (read-char stream nil nil))
		    (unless char
		      (when line
			(push line results))
		      (return-from response (values nil (nreverse results))))))
	     
	       (start-new-line ()
		 `(progn 
		    (when line (push line results))
		    (setq line (make-array 128 :element-type 'character
					   :adjustable t :fill-pointer 0))))
	     
	       (collect ()
		 `(if* (char= char #\Newline)
		     then (go start-new-line)
		     else (vector-push-extend char line)
			  (next-char)
			  (go collect)))
	     
	       (prompt-check ()
		 `(when (char= (next-char) #\p)
		    (if* (not (char= (next-char) #\s))
		       then (vector-push-extend #\p line)
			    (vector-push-extend char line)
		     elseif (not (char= (next-char) #\f))
		       then (vector-push-extend #\p line)
			    (vector-push-extend #\s line)
		     elseif (not (char= (next-char) #\t))
		       then (vector-push-extend #\p line)
			    (vector-push-extend #\s line)
			    (vector-push-extend #\f line)		  
		     elseif (not (char= (next-char) #\p))
		       then (vector-push-extend #\p line)
			    (vector-push-extend #\s line)
			    (vector-push-extend #\f line)
			    (vector-push-extend #\t line)
		     elseif (not (char= (next-char) #\>))
		       then	(vector-push-extend #\p line)
			    (vector-push-extend #\s line)
			    (vector-push-extend #\f line)
			    (vector-push-extend #\t line)
			    (vector-push-extend #\p line)
		     elseif (not (char= (next-char) #\Space))
		       then (vector-push-extend #\p line)
			    (vector-push-extend #\s line)
			    (vector-push-extend #\f line)
			    (vector-push-extend #\t line)
			    (vector-push-extend #\p line)
			    (vector-push-extend #\> line)			
		       else ;; prompt found
			    (return-from response (values t (nreverse results)))))))
      
      (block response
	(tagbody
	
	 start-new-line
	
	  (start-new-line)
	  
	  (prompt-check)
	  
	 collect
	  
	  (collect))))))

(defun connect-to-sftp-server (host &key (user net.ftp.client::*default-user*)
					 (password net.ftp.client::*default-password*)
					 port
					 debug
					 (private-key (util.string:string+ (sys:getenv "HOME") "\\.ssh\\id_rsa.ppk")))
  (let ((command (format nil "psftp.exe ~{-i ~A~} ~{-P ~A~} ~{~A@~}~A"
			 (when private-key
			   (list private-key))
			 (when port
			   (list port))
			 (when user
			   (list user))
			 host)))
    (run-shell-command command
		       :show-window :hide
		       :input :stream
		       :output :stream
		       :separate-streams nil
		       :wait nil)))

(defconstant +pwd-re+
    "^Remote working directory is \\(.+\\)$")

(defconstant +listing-directory-re+
    "^Listing directory \\(.+\\)$")

(defun map-over-sftp-directory (function host remote-path
				&key (recurse t)
				     port
				     (user net.ftp.client::*default-user*)
				     (password net.ftp.client::*default-password*)
				     (private-key (util.string:string+ (sys:getenv "HOME") "\\.ssh\\id_rsa.ppk"))
				     include-directories
				     debug
				     ignore-mtime)
  (multiple-value-bind (stream pid)
      (connect-to-sftp-server host :port port :user user
			      :private-key private-key)
    (let ((ok?)
	  (response))
      (multiple-value-setq (ok? response) (wait-for-response stream))
      (when debug
	(mapcar #'(lambda (line) (princ line) (terpri)) response))
      (if* ok?
	 then (net.ftp.client::ftp-command stream "cd ~A" remote-path)
	      (multiple-value-setq (ok? response) (wait-for-response stream))
	      (when debug
		(mapcar #'(lambda (line) (princ line) (terpri)) response))
	      (if* ok?
		 then (net.ftp.client::ftp-command stream "dir")
		      (multiple-value-setq (ok? response) (wait-for-response stream))
		      (when debug
			(mapcar #'(lambda (line) (princ line) (terpri)) response))
		      (if* ok?
			 then (multiple-value-bind (match-p whole directory)
				  (match-regexp +listing-directory-re+ (first response))
				(if* match-p
				   then (setq directory (excl::unix-namestring (pathname-as-directory directory))
					      response (rest response))
				   else (setq directory (excl::unix-namestring (pathname-as-directory remote-path))))
				(mapcar #'(lambda (line)
					    (process-directory-listing line function directory)) response))
			      (multiple-value-setq (ok? response) (net.ftp.client::ftp-command stream "quit"))
			      (when debug
				(mapcar #'(lambda (line) (princ line) (terpri)) response))
			      (if* (not ok?) ;; we don't want a prompt!
				 then (close stream)
				      (sys:reap-os-subprocess :pid pid)
				      (values)
				 else (error "Could not shut down psftp.exe"))
			 else (error "Could not get directory listing for ~A" remote-path))
		 else (error "Could not change to remote directory ~A" remote-path))
	 else (error "Could not connect to server.")))))

(defconstant +file-type-re+
    "^[bcCdDlMnpPs?-]")

(defconstant +mode-bits-re+
    "[r\\-][w\\-][x\\-][r\\-][w\\-][x\\-][r\\-][w\\-][x\\-]")

(defconstant +num-hard-links-re+
    " +\\([0-9]+\\)")

(defconstant +user-re+
    " +\\([^ ]+\\)")

(defconstant +group-re+
    " +\\([^ ]+\\)")

(defconstant +bytes-re+
    " +\\([0-9]+\\)")

(defconstant +month-re+
    " +\\(Jan\\|Feb\\|Mar\\|Apr\\|May\\|Jun\\|Jul\\|Aug\\|Sep\\|Oct\\|Nov\\|Dec\\)")

(defconstant +day-re+
    " +\\([1-3][0-9]\\|[0-9]\\)")

(defconstant +year-or-time-re+
    " +\\([0-2][0-9]:[0-5][0-9]\\|[1-2][0-9][0-9][0-9]\\)")

(defconstant +filename-re+
    " +\\(.+\\)$")

(defun process-directory-listing (line function directory)
  (multiple-value-bind (match-p whole hard-links
			user group bytes month
			day time-or-year filename)
      (match-regexp (load-time-value (concatenate 'string
				       +file-type-re+
				       +mode-bits-re+
				       +num-hard-links-re+
				       +user-re+
				       +group-re+
				       +bytes-re+
				       +month-re+
				       +day-re+
				       +year-or-time-re+
				       +filename-re+))
		    line)
    (declare (ignore whole hard-links))
    (if* (null match-p)
       then (warn "Failed to parse: ~S" line)
       else (let* ((month-number (1+ (position month #("Jan" "Feb" "Mar"
						       "Apr" "May" "Jun"
						       "Jul" "Aug" "Sep"
						       "Oct" "Nov" "Dec")
					       :test #'string=)))
		   (time-p (position #\: time-or-year))
		   (year-number (if time-p
				    (nth 5 (multiple-value-list (get-decoded-time)))
				  (parse-integer time-or-year)))
		   (hour-number (if time-p
				    (parse-integer (subseq time-or-year 0 time-p))
				  0))
		   (min-number (if time-p
				   (parse-integer (subseq time-or-year (1+ time-p)))
				 0))
		   (day-number (parse-integer day)))
	      (funcall function
		       (concatenate 'string directory filename)
		       (parse-integer bytes)
		       (encode-universal-time 0 min-number
					      hour-number
					      day-number
					      month-number
					      year-number
					      excl::*time-zone*)
		       )))))