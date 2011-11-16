The AllegroCL Secure FTP (SFTP) client module for Windows

This document contains the following sections:

1. Introduction
2. Setup
3. The AllegroCL SFTP Client API
4. Examples
5. Notes

1. Introduction
---------------

The AllegroCL SFTP client module can be used to communicate with an
SFTP server.  This module is a wrapper around the program PSFTP which
is part of the PuTTY program suite.  Since psftp.exe is a 32 bit
Windows program, this client module currently is only available for
the Windows operating system (32 and 64 bit).  This module has been
tested with PuTTY's psftp.exe program version 0.60.

The AllegroCL SFTP client module can do the following:
- Copy files to and from remote hosts
- Walk remote file structures
- Access the file modification time and size of remote files

2. Setup
--------

Loading the SFTP module

Load the SFTP module by compiling and loading psftp.cl:

(load (compile-file "c:\\path\\to\\psftp.cl"))

Symbols naming functionality in the sftp module are in the
net.sftp.client package.

3. The AllegroCL SFTP Client API
--------------------------------

The following operators and variables are supported.

All operators are functions unless otherwise indicated.

Operators that open and/or close an SFTP connection

- connect-to-sftp-server
- with-open-sftp-connection
- close-sftp-stream

Operators that act on an open SFTP connection

- sftp-stream-pwd
- sftp-stream-cwd
- sftp-stream-get
- sftp-stream-put
- sftp-stream-map-over-directory

Other operators

- sftp-get
- sftp-put
- map-over-sftp-directory

Variables

- *default-user*
- *default-password*
- *default-private-key*
- *sftp-debug*

Classes

- failed-connection
- file-exists-error

-----------------------

connect-to-sftp-server

Function

Package: net.sftp.client

Arguments: host &key user password private-key port debug verbose
	   	     mode enable-compression-p trust-new-host
		     error-timeout

connect-to-sftp-server creates and returns a command stream to a
psftp.exe instance which is connected to SFTP server on /host/ and
/port/ using /user/ and either /private-key/ or /password/ to
authenticate the session.  /user/ must be a string, /password/ is
either a string or `nil', /private-key/ is either a pathname, a string
representing a pathname, or `nil'.  Their default values are
*default-user*, *default-password*, and *default-private-key*
respectively.

If /port/ is not given, it defaults to psftp.exe's default port.
/debug/ and /verbose/ can be either `t' or `nil' and can help
troubleshoot problems with connecting.  (Note, however that use of
/verbose/ can cause problems of it's own with the command i/o.)
/enable-compression-p/ can be `t' or 'nil' and will enable or disable
compression in psftp.exe.  /trust-new-host/, when set to `t' is useful
for connecting to a host for the first time. (This allows the client
module to answer yes to the question of whether to trust a new host -
a failed-connection error will be signalled if psftp.exe replies with
this question and /trust-new-host/ is nil.)  /error-timeout/ is an
integer number of seconds that the client module should wait for an
error response from psftp.exe.  The default is 60 seconds.  /mode/
does nothing and is included for compatibility with net.ftp.client ftp
module.

The stream can be closed, and psftp.exe terminated with
close-sftp-stream.  Note that unlike the ftp module, the sftp stream
cannot be closed by cl:close alone, since it also has an error stream
and Windows process associated with it.

If an error occurs while establishing the connection, the stream is
closed and an error of type failed-connection is signalled.

-----------------------

with-open-sftp-connection

Macro

Package: net.sftp.client

Arguments: (var host &key port user password private-key
	   	     	  trust-new-host mode debug verbose)
	   &body body

with-open-sftp-connection establish a command stream (bound to /var/)
to a psftp.exe instance which is connected to an SFTP server using
connect-to-sftp-server.  See connect-to-sftp-server for an explanation
of the arguments.  /body/ is the code that is intended to run with the
open connection bound to the symbol given as /var/.  The value
returned is the value returned by /body/.

The value of opening a connection to an SFTP server using this macro
is that multiple commands can be sent to the same server without
having to close and reopen the connection. When using a heavily used
SFTP server it might be quite difficult to make a connection, so
having to reestablish it might not be a good idea.

Functions in this package whose names start with ``sftp-stream-''
operate on open SFTP sessions.

-----------------------

close-sftp-stream

Function

Package: net.sftp.client

Arguments: stream

close-sftp-stream will close an sftp stream.  This function also
terminates the psftp.exe process and closes the error stream.  This
function should be used instead of cl:close to end a connection opened
with connect-to-sftp-server.

-----------------------

sftp-stream-pwd

Function

Package: net.sftp.client

Arguments: stream &key debug

sftp-stream-pwd returns the working directory of a command stream
(sftp connection).  Use this to determine which directory is the
current durectory according to psftp.exe's sftp connection.

-----------------------

sftp-stream-cwd

Function

Package: net.sftp.client

Arguments: stream remote-path &key debug

sftp-stream-cwd changes the working directory of a command stream
(sftp connection).  Use this to traverse remote directories.
/remote-path/ should be a string or pathname.  Upon success
sftp-stream-cwd returns a string representation of the current
directory or else signals an error on failure.

-----------------------

sftp-stream-get

Function

Package: net.sftp.client

Arguments: stream remote-path local-filespec &key if-exists mode debug

sftp-stream-get transfers a remote file to the local filesystem using
an open command stream (sftp connection).  /stream/ is an sftp
connection such as that returned by connect-to-sftp-server.
/remote-path/ is a string or pathname representation of the remote
file to get.  /local-filespec/ is a string representing a local
filesystem pathname or an actual pathname.  If /if-exists/ is :error
(the default), then if the /local-filespec/ already exists, this
function will signal a file-exists-error.  /mode/ is ignored, and
/debug/ prints useful information to debug the command.  The return
value, upon success is a pathname of the local-file.

----------------------

sftp-stream-put

Function

Package: net.sftp.client

Arguments: stream local-filespec remote-path &key mode debug

sftp-stream-put transfers a local-file to the remote sftp server using
an open connection stream.  /stream/ is an sftp connection such as
that returned by connect-to-sftp-server.  /local-filespec/ is a
pathname or string representation of a pathname of an existing file on
the local filesystem.  An error is signalled if /local-filespec/ does
not exist. remote-path is a string and represents the remote filename
of where to put the file.  The remote-path can be relative of
absolute, so long as the location referred to by the absolute path
actually exists.  If the put command fails, lisp will signal an error.
The return value is a string representing the path of the remote file
as considered by the remote server.  /mode/ is ignored.

---------------------

sftp-stream-map-over-directory

Function

Package: net.sftp.client

Arguments: stream function remote-path &key (pass-connection-stream t)
	   	  	   	       	    (recurse t)
					    (include-directories nil)
					    (debug nil) ignore-mtime

sftp-stream-map-over-directory is a workhorse function of the sftp
client module.  With this function, one can traverse directories or
directory trees and operate on the remote files, transfering them to
the client machine or uploading new versions of files.  /stream/ is an
sftp connection stream (aka psftp.exe command stream) such as that
returned by connect-to-sftp-server.  /function/ is a callback
function.  /function/ takes three variables if
/pass-connection-stream/ is `nil' and /function/ takes four variables
if /pass-connection-stream/ is non-nil.  /remote-path/ is either a
pathname of a directory or string representation of the pathname of a
directory on the remote host.

/function/ is provided by the API user and has two signatures:

(remote-pathname file-size-bytes file-modification-time) 
[when pass-connection-stream is `nil']

or 

(remote-pathname file-size-bytes file-modification-time sftp-connection-stream)
[when pass-connection-stream is non-nil]

The first form is for compatibilty with net.ftp.client
map-over-ftp-directory function.

The second form is a new form which allows the user to operate on the
sftp-connection-stream in side their callback function.

When /recurse/ is non-nil, sftp-stream-map-over-directory will descend
into subdirectories and map over them.  When /include-directories/ is
non-nil, directories themselves will be passed into the callback
function.  When a directory is passed into a callback function, it's
file-size-bytes will be nil instead of an integer.  If
/include-directories/ is not nil then ensure your callback function
does not depend on file-size-bytes being an integer.  In the ftp
client module file-modification-time is also nil in this circumstance,
but for the sftp client module it was considered useful for the
file-modification-time of a directory to be a non-nil actual lisp
universal time.  When /debug/ is non-nil, output from the psftp.exe
commands and responses will be printed to *sftp-debug*, which is
either a stream, t or nil.  /ignore-mtime/ is ignored.

See examples below on how to use the map-over-directory functions.

---------------------------

sftp-get

Function:

Package: net.sftp.client

Arguments: host remote-path local-filespec &key port user password
	   		    		   	private-key if-exists
						mode debug

The function sftp-get retrieves a file from a remote host via SFTP
into a file on the local filesystem.  The connection arguments /host/,
/port/, /user/, /password/, and /private-key/ are passed into
connect-to-sftp-server and have the same datatypes and default values.
/remote-path/, /local-filespec/ and /if-exists/ are passed to
sftp-stream-get and similarly have the same datatypes as that
function.  /debug/ is passed into both connect-to-sftp-server and
sftp-stream-get and has the same meaning as described for those
functions.  /mode/ is ignored and is there for compatibility reasons.

This function is the same as sftp-stream-get (described above) except
that it doesn't require a stream.

On success the local pathname of the transferred file is returned or
else an error is signalled.

---------------------

sftp-put

Function:

Package: net.sftp.client

Arguments: host local-filespec remote-path &key port user password
	   		       		   	private-key mode debug

The function sftp-put uploads a file from the local host via SFTP to a
file on the remote host.  The connection arguments /host/, /port/,
/user/, /password/, /private-key/ are passed into
connect-to-sftp-server and have the same datatypes and default values
as the similarly named arguments from connect-to-sftp-server.
/local-filespec/ and /remote-path/ are passed into sftp-stream-put and
have the same datatypes, default values and semantics as the similarly
named arguments for sftp-stream-put.  /debug/ is passed into both
connect-to-sftp-server and sftp-stream-put and has the same meaning as
described for those functions.  /mode/ is ignored and is there for
compatibility reasons.

This function is the same as sftp-stream-put (described above) except
that it doesn't require a stream.

On success a string representation of the remote pathname is returned
or else an error is signalled.

---------------------

map-over-sftp-directory

Function:

Package: net.sftp.client

Arguments: function host remote-path &key port user password private-key
	   	    	 	     	  pass-connection-stream recurse
					  include-directories trust-new-host
					  verbose ignore-mtime debug

The function map-over-sftp-directory traverses (possibly recursively)
a remote directory and calls a user supplied function /function/ as a
callback for each item (file or directory).  Please note: the defaults
*are not* the same for map-over-sftp-directory as for
sftp-stream-map-over-directory.  /pass-connection-stream/ and
/recurse/ both have default values of `nil' in this version of the
function.  The reason for the former is compatibility with callback
function signatures for the net.ftp.client module, and the reason for
the second is to avoid possibly long or even recursively-linked
directory structures on the SFTP server. /port/, /user/, /password/,
/private-key/, /trust-new-host/ and /verbose/ have the same semantics,
datatypes and default values as for connect-to-sftp-server.
/function/, /remote-path/, /pass-connection-stream/, /recurse/ and
/include-directories/ have the same semantics and datatypes (but
different default values) as sftp-stream-map-over-directory.  /debug/
is the same as described in connect-to-sftp-server and
sftp-stream-map-over-directory.  /ignore-mtime/ is ignored, and
/verbose/ is available but not recommended.

See the examples below for explanation of how to use this function.

---------------------

*default-user*

Variable

Package: net.sftp.client

The value of this variable will be used as the default value to
functions which accept user as a keyword argument.  It is suggested
that it be bound for the duration of the interaction with a particular
sftp server.  It has no initial binding, it's value should be a
string.

---------------------

*default-password*

Variable

Package: net.sftp.client

The value of this variable will be used as the default value to
functions which accept a password as a keyword argument.  It is
suggested that a *default-private-key* be used instead, else it could
be bound for the duration of the interaction with a particular sftp
server.  It's initial value is `nil', it's value should be a string if
non-null.

---------------------

*default-private-key*

Variable

Package: net.sftp.client

The value of this variable will be the (local) pathname supplied to
psftp.exe to the PuTTY private key.  The private key is not
transferred to the client, but it is used in authentication.  Username
and private-key are the preferred methods of authentication when using
this SFTP module.  *default-private-key* is initially unbound.  The
value should be bound to the symbol before using the module functions
(or :private-key should be explicitly given to the functions).
---------------------

*sftp-debug*

Variable

Package: net.sftp.client

The value of this variable should be either nil, t or a stream where
to direct debug output.  nil is the default value.

---------------------

failed-connection

Class

Package: net.sftp.client

This is the condition signalled when connect-to-sftp-server fails.

---------------------

file-exists-error

Class

Package: net.sftp.client

This is the condition signalled when attempting to get a file which
already exists on the local filesystem.

4. Examples
-----------

Prerequisite:

psftp.exe must be installed and must be accessible via the %PATH%
environment variable for this module to work.

Suggested practice: Use a public/private key pair instead of a
password.

Use puttygen.exe to either convert an OpenSSH private key to PuTTY's
format (recommended), or create a native PuTTY public/private key
pair.  If the server to be used is an OpenSSH server, then on the
server you can append the public key to the authorized_keys file of
the user's .ssh file of the user you intend to connect as.  If this
file is an OpenSSH key then this can be done directly (i.e. cat
id_dsa.pub >> .ssh/authorized_keys) or you can follow the PuTTY
instructions for puttygen generated public keys.

It is suggested to put the private key in
%HOME%\.ssh\id_[dsa|rsa].ppk, this is the default location where the
lisp client will look for the key.  Note: the key is not transmitted
to any clients, the psftp.exe program uses it for the authentication
procedure only.

Alternate practice:

If you decide not to use a public/private key pair, you must use a
password.  This is not recommended since the password will be
clear-text in your code!

;; example:

;; run this once, with your login name:

(net.sftp.client::setup-defaults "<username>")

;; setup-defaults is provided as a convenience for testing and
;; demonstration purposes and should be replaced with your own default
;; setup function in your application

;; next define a test callback function to pass to map-over-sftp-directory:

(defun mapdir-test-fn1 (remote-pathname file-size file-mod-time &optional stream)
  (print (list remote-pathname file-size file-mod-time stream)))

;; execute this code at a lisp prompt replacing "gemini" with your
;; server name:

(net.sftp.client:map-over-sftp-directory
 #'mapdir-test-fn1 "<remote-host>" "." 
 :include-directories t :pass-connection-stream t)

;; it should print a number of lists:
;; ("/home/username/test" 534 3494044800 #<terminal-simple-stream  fd 9/8 @ #x101103a92>)

;; this is the printed representation of the arguments your callback
;; function recieves:
;; 1) remote-pathname (a string)
;; 2) file-size-bytes (an integer number of bytes)
;; 3) file-modification-date (a lisp universal time
;; representing modification time to the granularity provided by psftp.)
;; when you :include-directories t, you might get `nil' for the file size, 
;; this means the remote-pathname is a directory

5. Notes
--------

psftp.exe must be located in a directory on the %PATH% environment
variable.

Runs on Windows implementations of AllegroCL.

if you don't have a %HOME%\.ssh\id_dsa.ppk then:
(net.sftp.client::setup-defaults "<username>" :key-file "c:\\path\\to\\private-key.ppk")

net.sftp.client:map-over-sftp-directory is different than
net.ftp.client:map-over-ftp-directory in two major ways.  First, the
connection-stream is optionally passed to the callback function so
that the callback function may make a decision about the arguments
(such as "have the file-size-bytes and/or file-mod-time changed since
last encountered?") and then decide to get [or put] that
remote-pathname using sftp-stream-get [or sftp-stream-put] in one
pass, with a single connection and authentication.

Second, remote-pathname is a string, not a pathname.  The reason for
this is that the SFTP client is designed to operate on Windows OS and
certain pathname function, such as `pathname' and
`pathname-to-directory' use `parse-pathname' which on Windows will
cause the forward slashes to be converted to backward slashes, which
if passed back into the SFTP client may confuse Putty sFTP.  Remote
file pathname syntax should be preserved, although this implementation
assumes the remote is a UNIX host.  Furthermore, there can be
characters in unix pathnames that cause the Windows implementation of
`parse-pathname' to error, these characters include but are not
limited to the colon character `:'.
