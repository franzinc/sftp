(in-package :cl-user)

(eval-when (compile load eval)
  (require :util-string))

(defpackage :net.sftp.client
  (:use #:excl #:common-lisp)
  (:export #:connect-to-sftp-server
	   #:close-sftp-stream
	   #:with-open-sftp-connection
	   #:sftp-stream-pwd
	   #:sftp-stream-cwd
	   #:sftp-stream-get
	   #:sftp-stream-put
	   #:sftp-stream-map-over-directory
	   #:sftp-get
	   #:sftp-put
	   #:map-over-sftp-directory
	   #:*default-user*
	   #:*default-password*
	   #:*default-private-key*
	   #:*sftp-debug*
	   #:failed-connection
	   #:file-exists-error))

(in-package :net.sftp.client)


(defvar *default-user*)
(defvar *default-password*)
(defvar *default-private-key*)
(defvar *sftp-debug* nil)

(defconstant +store-key-in-cache-prompt+
    "
Store key in cache? (y/n) ")

(defun issue-psftp-command (stream &rest format-args)
  ;; send a command to the sftp server
  (let ((command (apply #'format nil format-args)))
    (if* *sftp-debug*
       then (format *sftp-debug* "command to psftp: ~s~%" command))
    (write-string command stream)
    (write-char #\return stream)
    (write-char #\newline stream)
    (force-output stream)))

(defun ensure-local-filespec (local-filespec)
  (setq local-filespec (pathname local-filespec))
  (let ((result (probe-file local-filespec)))
    (if* result
       then result
       else (error 'file-error
		   :format-control "~S was not found on the local filesystem."
		   :format-arguments (list local-filespec)
		   :pathname local-filespec))))

(defun ensure-trailing-slash (namestring)
  "doesn't use parse namestring like pathname-as-directory"
  (let* ((string (format nil "~A" namestring))
	 (length (length string)))
    (if* (zerop length)
       then string
     elseif (char= (char string (1- length)) #\/)
       then string
       else (util.string:string+ string #\/))))

(ff:def-foreign-call TerminateProcess ((handle win:win-handle) (exit-code win:uint))
  :returning (win:bool))

(defconstant PROCESS_TERMINATE 1)
(defconstant FALSE 0)

(defun windows-os-process-kill (process-id &optional (code 1))
  (let ((process-handle (win:OpenProcess PROCESS_TERMINATE FALSE process-id)))
    (if* (eql 0 process-handle)
       then (warn "Could not get process-handle for process-id ~A" process-id)
	    nil
       else (TerminateProcess process-handle code))))

(defmacro with-psftp-abort-on-error ((psftp) &body body)
  (let ((psftp-stream-var (gensym "PSFTP-")))
    `(let ((,psftp-stream-var ,psftp))
       (handler-bind ((error (lambda (e)
			       (force-psftp-shutdown ,psftp-stream-var)
			       (signal e))))
	 ,@body))))

(defun (setf psftp-errout) (value psftp)
  (with-stream-class (stream psftp)
    (setf (getf (sm excl::plist psftp) :psftp-errout) value)))

(defun psftp-errout (psftp)
  (with-stream-class (stream psftp)
    (getf (sm excl::plist psftp) :psftp-errout)))

(defun (setf psftp-procid) (value psftp)
  (with-stream-class (stream psftp)
    (setf (getf (sm excl::plist psftp) :psftp-procid) value)))

(defun psftp-procid (psftp)
  (with-stream-class (stream psftp)
    (getf (sm excl::plist psftp) :psftp-procid)))

(defun manage-error-response (psftp &key (timeout 60) 
					 (trust-new-host t)
					 (verbose nil))
  ;; timeout 60 is long enough to pass through a connection timeout error
  ;; (but short enough to not to halt application)
  (let* ((stream psftp)
	 (error-stream (psftp-errout psftp))
	 (input-ready-streams
	  (mp:wait-for-input-available 
	   (list error-stream stream)
	   :whostate "Waiting for initial psftp.exe response"
	   :timeout timeout)))
    (if* (not (find error-stream input-ready-streams))
       then (if* (not (find stream input-ready-streams))
	       then (error 'failed-connection 
			   "Timed out after ~A seconds waiting for psftp.exe response."
			   timeout)
	       else (return-from manage-error-response (values)))
       else
	    (let ((char (read-char-no-hang error-stream nil nil)))
	      (if* (null char)
		 then (return-from manage-error-response (values))
		 else (let ((message (make-array 4096 :element-type 'character
						 :adjustable t :fill-pointer 1)))
			(setf (char message 0) char)
			(loop
			    do (mp:wait-for-input-available error-stream :timeout 1)
			       (setq char (read-char-no-hang error-stream nil nil))
			       (if* (null char)
				  then (if* trust-new-host
					  then (let ((len (length message)))
						 (if* (> len 27)
						    then (if* (string= +store-key-in-cache-prompt+
								       (subseq message (- len 27)))
							    then (when (or *sftp-debug* verbose)
								   (format (or *sftp-debug* t) message))
								 (issue-psftp-command stream "y")
								 (return-from manage-error-response (values))
							    else (error 'failed-connection "Unknown response: ~S" message))
						    else (error 'failed-connection "Unknown response: ~S" message)))
					  else (error 'failed-connection "Unknown response: ~S" message))
				  else (vector-push-extend char message)))))))))

(defun close-sftp-stream (sftp-stream)
  (attempt-normal-psftp-shutdown sftp-stream :force t))

(defun attempt-normal-psftp-shutdown (psftp &key (timeout 1)
						 (force nil))
  (unwind-protect
      (issue-psftp-command psftp "quit")
    (close (psftp-errout psftp))
    (close psftp)
    (let ((end (+ (* 1000 timeout) (get-internal-real-time)))
	  (pid (psftp-procid psftp)))
      (loop
	  do (when (sys:reap-os-subprocess :pid pid :wait nil)
	       (return t))
	     (when (> (get-internal-real-time) end)
	       (if* force
		  then (when *sftp-debug*
			 (warn "Could not shut down psftp.exe normally, forcing..."))
		       (if* (force-psftp-shutdown psftp :close-streams nil)
			  then (return t)
			  else (return nil))
		  else
		       (return nil)))))))

(defun force-psftp-shutdown (psftp &key (close-streams t))
  (let ((procid (psftp-procid psftp)))
    (unwind-protect
	(when close-streams
	  (close (psftp-errout psftp))
	  (close psftp))
      (windows-os-process-kill procid 1)
      (sys:reap-os-subprocess :pid procid :wait nil))))

(defun debug-print-response (response)
  (when *sftp-debug*
    (fresh-line *sftp-debug*)
    (loop for line in response
	do (princ line *sftp-debug*)
	   (terpri *sftp-debug*))))

(defun wait-for-response (stream &key timeout)
  (let ((line)
	(char)
	(results ()))
    ;; todo: probably should make this function at least recognize
    ;; and deal with "user@host password: " which is a common cause of hangs
    (macrolet ((next-char ()
		 `(prog1 
		      (if* timeout ;; probably shouldn't use timeout.
			 then (mp:wait-for-input-available stream :timeout timeout)
			      (prog1 (setq char (read-char-no-hang stream nil nil))
				(unless char
				  (error 'failed-connection
					 "psftp.exe not responding:~{~%~A~}~%~A"
					 (nreverse results) line)))
			 else (setq char (read-char stream nil nil)))
		    (unless char
		      (when line
			(push line results))
		      (setq results (nreverse results))
		      (debug-print-response results)
		      ;; this case probably rarely happens, if at all
		      ;; perhaps this function should be reengineered
		      ;; to take a timeout which is either nil or seconds
		      ;; then mp:wait-for-input-available would be used
		      ;; along with read-char-no-hang.  The user could
		      ;; possibly provide the timeout which would get reset
		      ;; at every new character arriving.  Just brainstorming.
		      ;; (because this function will hang indefinitely at a problem)
		      (return-from response (values results :eof)))))
	     
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
			    (setq results (nreverse results))
			    (debug-print-response results)
			    (return-from response (values results nil))))))
      
      (block response
	(tagbody
	
	 start-new-line
	
	  (start-new-line)
	  
	  (prompt-check)
	  
	 collect
	  
	  (collect))))))

(defmacro with-command-response ((response-var
				  stream
				  &rest format-args)
				 &body body)
  `(let ((,response-var))
     (issue-psftp-command ,stream ,@format-args)
     (setq ,response-var (wait-for-response ,stream))
     ,@body))
  
  

(define-condition failed-connection (error) ())

(defun connect-to-sftp-server (host &key (user *default-user*)
					 (password *default-password*)
					 port
					 debug
					 verbose
					 mode
					 (private-key *default-private-key*)
					 enable-compression-p
					 trust-new-host
					 (error-timeout 60)
			       &aux (*sftp-debug* debug))
  (declare (ignore mode))
  (let ((command (format nil "psftp.exe~{ ~A~}~{ ~A~}~{ -i ~A~}~{ -P ~A~}~{ -pw ~A~} ~{~A@~}~A"
			 (when enable-compression-p
			   (list "-C"))
			 (when verbose
			   ;; verbose mode can cause problems!
			   (list "-v"))
			 (when private-key
			   (list private-key))
			 (when port
			   (list port))
			 (when password
			   (list password))
			 (when user
			   (list user))
			 host)))
    (when *sftp-debug*
      (format *sftp-debug* "psftp command: ~S" command))
    (multiple-value-bind (stream error-output pid)
	(run-shell-command command
			   :show-window :hide
			   :input :stream
			   :output :stream
			   :separate-streams nil
			   :error-output :stream
			   :wait nil)
      
      (setf (psftp-errout stream) error-output
	    (psftp-procid stream) pid)
      
      (with-psftp-abort-on-error (stream)
	
	;; psftp typically prints to error stream here
	;; so this function was made special to handle
	;; unusual circumstances gracefully (by entering
	;; the debugger and killing psftp instead of
	;; just hanging without providing any clues to
	;; what the trouble is)
	;; trust-new-host is the one smart handler that
	;; it is equipped with, the common scenario where
	;; the host asks to be recognized for the first
	;; time.  In the future a case could be added to
	;; handle a password or username prompt, though
	;; there might not be much value in that
	
	;; A user should run with :debug t for the first
	;; times connecting to a host, possibly providing
	;; :trust-new-host t to solve connection problems
	(manage-error-response stream
			       :timeout error-timeout
			       :verbose verbose
			       :trust-new-host trust-new-host)
	
	
	(let ((response (wait-for-response stream :timeout 1)))
	  (if* response
	     then stream
	     else (error 'failed-connection
			 :format-control "Failed to connect to sftp server.")))))))





	    

(defmacro with-open-sftp-connection ((var host
				      &key port
					   (user *default-user*)
					   (password *default-password*)
					   (private-key *default-private-key*)
					   trust-new-host
					   enable-compression-p
					   mode
					   debug
					   verbose)
				     &body body)
  `(let ((*sftp-debug* ,debug)
	 (,var))
     (unwind-protect
	 (progn
	   (setq ,var
	     (connect-to-sftp-server ,host 
				     :port ,port
				     :user ,user
				     :password ,password
				     :private-key ,private-key
				     :mode ,mode
				     :debug ,debug
				     :verbose ,verbose
				     :enable-compression-p ,enable-compression-p
				     :trust-new-host ,trust-new-host))
	   ,@body)
       (when ,var
	 (close-sftp-stream ,var)))))

  
  

(defconstant +pwd-re+
    "^Remote working directory is \\(.+\\)$")

(defconstant +listing-directory-re+
    "^Listing directory \\(.+\\)$")

(defconstant +cwd-re+
    "Remote directory is now \\(.+\\)$")

(defun sftp-stream-cwd (stream remote-path
			&key debug
			&aux (*sftp-debug* debug))
  (setq remote-path (excl::unix-namestring (ensure-trailing-slash remote-path)))
  
  (with-command-response (response stream "cd \"~A\"" remote-path)
    (multiple-value-bind (match-p whole directory)
	(match-regexp +cwd-re+ (first response))
      (declare (ignore whole))
      (if* match-p
	 then directory
	 else (error "Failed to change remote directory to ~S" remote-path)))))

(defun sftp-stream-pwd (stream
			&key debug
			&aux (*sftp-debug* debug))
  (with-command-response (response stream "pwd")
    (multiple-value-bind (match-p whole wd)
	(match-regexp +pwd-re+ (first response))
      (declare (ignore whole))
      (if* match-p
	 then wd
	 else (error "Failed to get the working directory of the sftp stream.")))))

  
  
(defun sftp-stream-map-over-directory (stream function remote-path
				       &key (recurse t)
					    (pass-connection-stream t)
					    (include-directories nil)
					    (debug nil)
					    ignore-mtime
				       &aux (*sftp-debug* debug)
					    (more-directories ()))
  (declare (ignore ignore-mtime))

  (when remote-path
    (unless (string= remote-path "")
      (setq remote-path (excl::unix-namestring (ensure-trailing-slash remote-path)))
      (sftp-stream-cwd stream remote-path :debug debug)))
  
  (with-command-response (response stream "dir")
    (multiple-value-bind (match-p whole directory)
	(match-regexp +listing-directory-re+ (first response))
      (declare (ignore whole))
      (if* match-p
	 then (setq directory (excl::unix-namestring (ensure-trailing-slash directory))
		    response (rest response))
	 else (error "Failed to get directory listing for remote-path ~S" remote-path))
      (loop for line in response
	  do (multiple-value-bind (filename
				   file-size-bytes
				   file-mod-time
				   item-is-directory-p)
		 (parse-psftp-directory-listing-line line)
	       (unless (or (string= filename ".")
			   (string= filename ".."))
		 (let ((remote-pathname
			(util.string:string+ directory filename)))
		   (if* item-is-directory-p
		      then (when recurse
			     (push remote-pathname more-directories))
			   (when include-directories
			     (apply function
				    remote-pathname
				    nil file-mod-time
				    (when pass-connection-stream
				      (list stream))))
		      else (apply function remote-pathname file-size-bytes file-mod-time
				  (when pass-connection-stream (list stream))))))))))
  
  (loop for remote-directory in more-directories
      do (sftp-stream-map-over-directory stream function remote-directory
					 :include-directories include-directories
					 :recurse t
					 :pass-connection-stream pass-connection-stream
					 :debug debug))
  
  (values))
					 


(defun map-over-sftp-directory (function host remote-path
				&key (recurse nil)
				     (pass-connection-stream nil)
				     port
				     (user *default-user*)
				     (password *default-password*)
				     (private-key *default-private-key*)
				     trust-new-host
				     include-directories
				     debug
				     verbose
				     ignore-mtime
				&aux (*sftp-debug* debug))  
  
  (with-open-sftp-connection (stream host
				     :port port
				     :user user
				     :password password
				     :private-key private-key
				     :debug debug
				     :verbose verbose
				     :trust-new-host trust-new-host)
    
    (sftp-stream-map-over-directory stream function remote-path
				    :include-directories include-directories
				    :recurse recurse
				    :pass-connection-stream pass-connection-stream
				    :debug debug
				    :ignore-mtime ignore-mtime))
  (values))

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
    "+\\(.+\\)")

(defconstant +eol-re+ "$")

(defconstant +space-re+ " ")

(defun parse-psftp-directory-listing-line (line)
  (let ((item-is-directory-p (and (> (length line) 0) (char= (char line 0) #\d))))
    
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
					 +space-re+
					 +filename-re+
					 +eol-re+))
		      line)
      (declare (ignore whole hard-links user group))
      (if* (null match-p)
	 then (warn "Failed to parse: ~S" line)
	      line
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
		(values filename
			(parse-integer bytes)
			(encode-universal-time 0 min-number
					       hour-number
					       day-number
					       month-number
					       year-number
					       excl::*time-zone*)
			item-is-directory-p))))))

(defconstant +local-re+
    "local\:")

(defconstant +remote-re+
    "remote\:")

(defparameter *get-cmd-response-re*
    (util.string:string+ #\^ +remote-re+ +filename-re+ " => " +local-re+ +filename-re+ +eol-re+))

(defparameter *put-cmd-response-re*
    (util.string:string+ #\^ +local-re+ +filename-re+ " => " +remote-re+ +filename-re+ +eol-re+))

(defun sftp-stream-put (stream local-filespec remote-path
			&key (mode :binary)
			     debug
			&aux (*sftp-debug* debug))
  (declare (ignore mode))
  (setq local-filespec (ensure-local-filespec local-filespec))
  (setq remote-path (excl::unix-namestring remote-path))
  (with-command-response (response stream "put \"~A\" \"~A\"" local-filespec remote-path)
    (multiple-value-bind (match-p whole local-file remote-file)
	(match-regexp *put-cmd-response-re* (first response))
      (declare (ignore whole local-file))
      (if* match-p
	 then remote-file
	 else (error "sftp-stream-put failed.")))))

(define-condition file-exists-error (error) ())

(defun sftp-stream-get (stream remote-path local-filespec
			&key (if-exists :error)
			     (mode :binary)
			     debug
			&aux (*sftp-debug* debug))
  (declare (ignore mode))
  (setq remote-path (excl::unix-namestring remote-path))
  (setq local-filespec (pathname local-filespec))
  (if* (and (eql if-exists :error)
	    (probe-file local-filespec))
     then (error 'file-exists-error
		 :format-control "Local file already exists: ~A"
		 :format-arguments (list local-filespec))
     else
	  (with-command-response (response stream "get \"~A\" \"~A\"" remote-path local-filespec)
	    (multiple-value-bind (match-p whole remote-file local-file)
		(match-regexp *get-cmd-response-re* (first response))
	      (declare (ignore whole remote-file local-file))
	      (if* match-p
		 then (probe-file local-filespec)
		 else (error "sftp-stream-get failed."))))))

#+#:DELETEME-later
(defun sftp-stream-file-mod-time (stream remote-path &key debug)
  (declare (ignore stream remote-path debug))
  (error "individual file mod time not supported by psftp"))

#+#:DELETEME-later
(defun sftp-file-mod-time (host remote-path &key port
						 user
						 password
						 private-key
						 debug)
  (declare (ignore host remote-path port user password private-key debug))
  (error "individual file mod time not supported by psftp."))

#+#:DELETEME-later
(defun sftp-size (host remote-path &key port
					user
					password
					private-key
					debug)
  (declare (ignore host remote-path port user password private-key debug))
  (error "individual file mod time not supported by psftp."))

#+#:DELETEME-later
(defun sftp-stream-size (stream remote-path &key debug)
  (declare (ignore stream remote-path debug))
  (error "individual file size not supported by psftp"))

(defun sftp-get (host remote-path local-filespec &key port 
						      debug
						      (if-exists :error)
						      (user *default-user*)
						      (password *default-password*)
						      (private-key *default-private-key*)
						      (mode :binary)
		 &aux (*sftp-debug* debug))
  (if* (and (eql if-exists :error)
	    (probe-file local-filespec))
     then (error 'file-exists-error
		 :format-control "Local file already exists: ~A"
		 :format-arguments (list local-filespec))
     else
	  (with-open-sftp-connection (stream host
					     :port port
					     :user user
					     :password password
					     :private-key private-key
					     :debug debug
					     :mode mode)
	    (sftp-stream-get stream remote-path local-filespec :if-exists if-exists))))

(defun sftp-put (host local-filespec remote-path &key port
						     (user *default-user*)
						     (password *default-password*)
						     (private-key *default-private-key*)
						     (mode :binary)
						      debug
		 &aux (*sftp-debug* debug))
  (with-open-sftp-connection (stream host
				     :port port
				     :user user
				     :password password
				     :private-key private-key
				     :debug debug
				     :mode mode)
    (sftp-stream-put stream local-filespec remote-path)))



(defun setup-defaults (user &key (key-type :dsa key-type-p)
				 (key-location (util.string:string+ (sys:getenv "HOME") "\\.ssh\\") key-location-p)
				 key-file
				 password
				 (debug nil debug-p))
  (when debug-p (setq *sftp-debug* debug))
  (if* (or (and key-file key-type-p)
	   (and key-file key-location-p))
     then (error "Ambiguous: cannot specify key-file and either key-type or key-location at the same time.")
   elseif (and (null key-file) (or (null key-type) (null key-location)))
     then (error "Underspecified: key-type or key-location is null")
     else (setq *default-user* user
		*default-password* password
		*default-private-key* (or key-file
					  (util.string:string+ key-location (ecase key-type
									      (:dsa "id_dsa.ppk")
									      (:rsa "id_rsa.ppk")))))
	  t))

#+#:IGNORE
(defun mapdir-test-fn1 (remote-pathname file-size file-mod-time &optional stream)
  (print (list remote-pathname file-size file-mod-time stream)))


