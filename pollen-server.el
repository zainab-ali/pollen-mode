(require 'project)
(require 'rx)
(require 'dash)
(require 's)

(defvar pollen-server-port 8080 "Port for Pollen project server.

See URL `https://docs.racket-lang.org/pollen/first-tutorial.html' for details on
the project sever.")

(defun pollen-start-server ()
  "Start a Pollen project server for the current project.

See URL `https://docs.racket-lang.org/pollen/first-tutorial.html' for details on
the project server.

The server runs in a *pollen* buffer on the port specified by
`pollen-server-port'. It is started in the project root directory unless an
info.rkt file specifies otherwise.

To start the server in a sub-directory, create an info.rkt file with a name
corresponding to that directory.  For example, given a project '~/foo'
with a subdirectory '~/foo/bar', write the following in '~/foo/info.rkt':

#lang info
(define name \"bar\")

The server is then started in the '~/foo/bar' directory.

The server is started using the 'raco pollen start' command, so you must install
raco to use it. See URL `https://docs.racket-lang.org/pollen/raco-pollen.html'
for details on setting up raco."
  (interactive)
  (unless
      (-when-let* ((project-root (pollen--project-root))
		   (*buffer* (get-buffer-create "*pollen*"))
		   (default-directory project-root)
		   (found-raco (pollen--found-raco))
		   (server-root (pollen--server-root)))
	(start-process "Pollen" *buffer*
		       "raco" "pollen" "start"
		       (file-relative-name server-root project-root)
		       (number-to-string pollen-server-port)))
    (message
     "Could not start server. Check the *pollen-log* for more details.")))

(defun pollen--found-raco ()
  "Check if the raco executable is installed."
  (let ((raco (executable-find "raco")))
    (if raco
	t
      (pollen--log "Could not find the 'raco' executable.
The pollen project server needs 'raco' to start.
Did you remember to install it? See https://docs.racket-lang.org/pollen/Installation.html for instructions.")
      )))


(defun pollen--project-root ()
  "Returns the project root directory, if it can be identified"
  (let ((project (project-current)))
    (if project
	(let ((roots (project-roots project)))
	  ;; If the project spans multiple directories, we can't decide which
	  ;; one to use. We return `nil' instead.
	  (if (equal (length roots) 1)
	      (car roots)
	    (pollen--log
	     (format "Expected a single root directory but found %i"
		     (length roots)))))
	(pollen--log "Unable to find current project with (project-current).")
	)))

(defun pollen--server-root ()
  "Directory in which the pollen project server is started.

This is usually just the project root directory. If there is an info.rkt file
in the root directory with a 'name' tag, this is taken as the subdirectory."
  (when-let* ((project-root (pollen--project-root)))
    (let ((name (pollen--info.rkt-name)))
      (if name (concat project-root name)
	project-root))))


(defun pollen-browse ()
  "Open the current buffer's page in the browser.

The current buffer must be a file-backed buffer containing pollen markup with a
'.pm' extension. The pollen project server serves a corresponding page
('foo.html.pm'is served as 'foo.html').  This page is opened in a browser using
 `browse-url'.

If there isn't a server running, an attempt is made to start one using
`pollen-start-server'.
"
  (interactive)
  (unless (process-live-p (get-process "Pollen"))
    (pollen--log "Attempting to start the pollen server to browse a URL.")
    (pollen-start-server))
  (if (process-live-p (get-process "Pollen"))
      (let* ((server-root (pollen--server-root))
	     (document (file-relative-name buffer-file-name server-root))
	     (path (pollen--url-path document)))
	(browse-url
	 (concat "http://localhost:" (number-to-string pollen-server-port) "/" path)))
    (pollen--log "Unable to browse the URL.  The pollen server failed to start.")
    (message "Unable to browse URL. Check the *pollen-log* for more details.")
    ))

(defun pollen--url-path (doc)
  "Find the url at which the server serves the document DOC.

The DOC is assumed to be pollen markup with a .pm extension. It's url is
stripped of this extension (e.g. 'foo.bar.pm' becomes 'foo.bar').
See URL `https://docs.racket-lang.org/pollen/third-tutorial.html' for details on
pollen markup files."
  (let* ((path-rx (rx (group (1+ anychar)) ".pm" eol))
	(match (s-match path-rx doc)))
    (if match (cadr match)
      (pollen--log
       (format
	"Unable to extract url for document %s. Is it a pollen markup file?"
	doc)))))

(defun pollen--info.rkt-name ()
  "Extracts the name tag out of info.rkt file.

This is used to specify a sub-directory in which the server is started.

The 'info.rkt' file is optional, as is it's name tag. This returns nil if the
file or name tag are missing."
  (-when-let* ((root (pollen--project-root))
	     (info (concat root "info.rkt"))
	     (found-info (file-exists-p info))
	     (name-rx (rx "(define"
			  (1+ " ")
			  "name"
			  (1+ " ")
			  ?\"
			  (group (1+ (not (any ?\"))))
			  ?\")))
    (with-temp-buffer
      (insert-file-contents info)
      (goto-char (point-min))
      (search-forward-regexp name-rx)
      (let ((name (match-string 1)))
	(if name name
	  (pollen-log "Unable to extract name from 'info.rkt' file."))))))

(defun pollen--log (message)
  "Write a message to the *pollen-log* buffer"
  (with-current-buffer (get-buffer-create "*pollen-log*")
    (goto-char (point-max))
    (insert message)
    (newline)))

(provide 'pollen-server)
