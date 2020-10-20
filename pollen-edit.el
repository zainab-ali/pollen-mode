(defun pollen-change-tag-name (next)
  (interactive "s")
  (pcase-let ((`(,start . ,end) (bounds-of-thing-at-point 'pollen-tag)))
    (delete-region start end)
    (goto-char start)
    (insert next)))

(defun pollen-surround (start end next)
  "Surrounds selected region with a pollen tag"
  (interactive "r\ns")
  (goto-char end)
  (insert "}")
  (goto-char start)
  (insert (format "◊%s{" next))
  )

(defun pollen-delete-surrounding-tag ()
  "Deletes the surrounding tag, keeping the content"
  (interactive)
  (let ((tag (pollen--tag-surrounding-point)))
    (if tag
	(progn
	  (goto-char (pollen--tag-lozenge tag))
	  (delete-region (pollen--tag-lozenge tag)
			 (pollen--tag-closing-brace tag))
	  (insert (pollen--tag-content tag)))
      (signal 'scan-error '("No surrounding tag")))))

(defun pollen-mark-surrounding-content ()
  "Mark the content of the surrounding tag"
  (interactive)
  (let ((tag (pollen--tag-surrounding-point)))
    (if tag
	(progn
	  (set-mark (pollen--tag-content-start tag))
	  (goto-char (pollen--tag-content-end tag))
	  (activate-mark))
      (signal 'scan-error '("No surrounding tag")))))

(defun pollen-change-surrounding-tag-name (next)
  (interactive "s")
  (let ((tag (pollen--tag-surrounding-point))
	 (start (point)))
     (if tag
	 (progn
	   (push-mark (point) nil)
	   (goto-char (pollen--tag-name-start tag))
	   (delete-region (pollen--tag-name-start tag) (pollen--tag-name-end tag))
	   (insert next)
	   (goto-char (mark))
	   (pop-mark))
       (signal 'scan-error '("No surrounding tag")))))

(defun pollen-split ()
  "Splits the content into two tags around the point."
  (interactive)
  (let ((tag (pollen--tag-surrounding-point)))
    (if tag
	(progn
	    (push-mark (pollen--tag-closing-brace tag) nil)
	    (kill-region (point) (pollen--tag-content-end tag))
	    (goto-char (mark))
	    (insert (format "\n◊%s{%s}" (pollen--tag-name tag)
			    (s-trim (pop kill-ring))))
	    (pop-mark)))
    (signal 'scan-error '("The point is not within a tag.  Unable to split."))
    ))

(defun pollen-join ()
  (interactive)
  (let ((tag (pollen--tag-surrounding-point)))
    (if tag
	(let ((text-and-next-tag
	       (save-excursion
		 ;; Move the point just after the closing brace
		 (goto-char (pollen--tag-closing-brace tag))
		 ;; Move forward to the next tag
		 (forward-thing 'pollen-tag 1)
		 ;; If we didn't move, there wasn't a next tag.  Return nil
		 (unless (equal (pollen--tag-closing-brace tag) (point))
		   ;; Move directly after the opening brace
		   (forward-thing 'pollen-opening-brace 1)
		   (let ((next-tag (pollen--tag-surrounding-point)))
		     ;; Get the text between the previous tag and this one
		     (cons (buffer-substring
			    (pollen--tag-closing-brace tag)
			    (pollen--tag-lozenge next-tag))
			   next-tag))))))
	  (if text-and-next-tag
	      (pcase-let ((`(,text . ,next-tag) text-and-next-tag))
		(if (string-empty-p (s-trim text))
		    (progn
		      ;; Delete the next tag
		      (delete-region
		       (- (car (pollen--tag-name-bounds next-tag) ) 1)
		       (+ (cdr (pollen--tag-content-bounds next-tag) ) 1))
		      ;; add the next tag contents onto the end of this one
		      (goto-char (cdr (pollen--tag-content-bounds tag)))
		      (insert " ")
		      (insert (pollen--tag-content next-tag))
		      )
		  (signal 'scan-error `(,(format "There is text between this tag and the next [%s].  Unable to join."
						 (s-trim text))))
		  ))
	    (signal 'scan-error '("There is no tag following this one.  Unable to join."))))
      (signal 'scan-error '("The point is not within a tag.  Unable to join.")))
    ))

(define-key pollen-markup-mode-map (kbd "C-c j") 'pollen-join)
(define-key pollen-markup-mode-map (kbd "C-c s") 'pollen-split)
(define-key pollen-markup-mode-map (kbd "C-c c") 'pollen-change-surrounding-tag-name)
(define-key pollen-markup-mode-map (kbd "C-c d") 'pollen-delete-surrounding-tag)
