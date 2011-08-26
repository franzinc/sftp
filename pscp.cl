(in-package #:net.sftp.client)

(defun pscp-get (remote-host filename &key
				      (verbose nil)
				      username
				      password
				      remote-dir
				      local-dir
				      ppk-file
				      (protocol nil)
				      (remote-type :unix))
  (declare (ignore password))
  
  (flet ((get-from-shell (stream)
	   (prog1
	       (with-output-to-string (out)
		 (do ((ch (read-char stream nil :eof)
			  (read-char stream nil :eof)))
		     ((eql :eof ch))
		   (write-char ch out)))
	     (close stream))))
    
    (let ((command (apply #'concatenate 'string
			  "pscp "
			  (append 
			   (when protocol
			     (list
			      (ecase protocol
				(:scp "-scp ")
				(:sftp "-sftp "))))
			   (if* ppk-file
			      then (list "-i " ppk-file " ")
			      else (list "-i " (sys:getenv "HOME")
					 "\\.putty\\id_dsa.ppk "))
			   (when username
			     (list username "@"))
			   (list remote-host ":")
			   (when remote-dir
			     ;; this must have the forward or backward
			     ;; slash convention of the remote host os
			     (let ((remote-directory
				    (pathname-as-directory remote-dir)))
			       (list
				(ecase remote-type
				  (:unix (excl::unix-namestring remote-directory))
				  (:dos (excl::directory-to-dos-namestring
					 remote-directory))))))
			   (list filename " ")
			   (if* local-dir
			      then (list (namestring
					  (pathname-as-directory local-dir)))
			      else (list (setq local-dir ".")))))))
      (when verbose
	(print command))
      
      (multiple-value-bind (input shell-stream error-output pid)
	  (excl:run-shell-command 
	   command
	   :input :stream
	   :output :stream
	   :error-output :stream
	   :separate-streams t
	   :show-window :hide
	   :wait nil)

	(setf (eol-convention shell-stream) :anynl-dos)

	;; probably can still read these streams after subprocess is reaped
	;; if so, avoid reading them unless necessary. (but close them instead)
	(let ((output (get-from-shell shell-stream))
	      (errout (get-from-shell error-output)))

	  (close input)
	  (multiple-value-bind (result pid signal)
	      (sys:reap-os-subprocess :pid pid)
	    (declare (ignore pid signal))
	    (if* result
	       then (if* (eq result 0)
		       then ;; command completed successfully
			    (when verbose
			      (format t "~A" output))
			    (probe-file
			     (merge-pathnames
			      filename
			      (pathname-as-directory local-dir)))
		       else (error "pscp returned error code ~A: ~A"
				   result errout))
	       else (error "Could not reap subprocess."))))))))