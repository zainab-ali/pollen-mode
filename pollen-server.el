(require 'project)
(require 'rx)

(defvar pollen-server-port
  8080
  "Port for Pollen server")

(cl-defun pollen-browse ()
  "Open a page to the active pollen server"
  (interactive)
  (let*
      ((project-root
	(car (project-roots
	      (project-current))))
       (document (file-relative-name
		  buffer-file-name
		  (concat project-root (pollen--info-name)))))
    (browse-url
     (concat
      "localhost:"
      (number-to-string pollen-server-port)
      "/"
      ;;TODO: regex match
      (substring document 0 (- (length document) 3))))))

(cl-defun pollen-start-server ()
  "Start a Pollen project server in the current project."
  (interactive)
  (let*
      ((project-root
	(car (project-roots
	      (project-current))))
       (*buffer* (get-buffer-create "*pollen*"))
       (default-directory project-root))
    (start-process
     "Pollen" *buffer* "raco" "pollen" "start" (pollen--info-name) (number-to-string pollen-server-port))))
(cl-defun pollen--info-name ()
  "Extracts the name field out of info.rkt file"
  (let*
      ((project-root
	(car (project-roots
	      (project-current))))
       (info (concat project-root "info.rkt")))
    (with-temp-buffer
      (insert-file-contents info)
      (goto-char (point-min))
      (search-forward-regexp
       (rx "(define"
	   (1+ " ")
	   "name"
	   (1+ " ")
	   ?\"
	   (group (1+ (not (any ?\"))))
	   ?\"))
      (match-string 1))))

(provide 'pollen-server)
