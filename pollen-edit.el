(require 'dash)
(require 'pollen-tag)
(require 'ert)

(defun pollen-delete-surrounding-tag ()
  "Deletes the surrounding tag, keeping the content

For example, if the point position is `|'

 Before: ◊tag{conten|ts}
 After : contents"
  (interactive)
  (let ((tag (pollen--tag-surrounding-point)))
    (if tag
	(progn
	  (delete-region (- (pollen--tag-closing-brace tag) 1)
			(pollen--tag-closing-brace tag))
	  (delete-region (pollen--tag-lozenge tag)
			 (+ (pollen--tag-opening-brace tag) 1)))
      (signal 'scan-error '("No surrounding tag")))))

(ert-deftest pollen--delete-surrounding-tag-test ()
  (let ((texts
	 '(("◊tag{conten|ts}" "conten|ts")
	   ("◊tag{more ◊tagged{con|tents} inside}" "◊tag{more con|tents inside}")
	   ("◊tag{m|ore ◊tagged{contents} inside}" "m|ore ◊tagged{contents} inside")
	   ("◊tag{more ◊tagged{contents} i|nside}" "more ◊tagged{contents} i|nside" )
	   ;; TODO: Test errors
	   ;; ("◊ta|g{contents}" )
	   ;; ("◊ta|g{contents")
	   )))
    (--each texts
      (point-test-edit (car it)
		       (cadr it)
		       'pollen-delete-surrounding-tag)
      )))

(defun pollen-mark-surrounding-content ()
  "Mark the content of the surrounding tag.

Highlights the region within the tag braces by placing the mark after
the first brace and the point before the last.

For the tag
  ◊foo{This is some text}
the highlighted region would be
       This is some text
"
  (interactive)
  (let ((tag (pollen--tag-surrounding-point)))
    (if tag
	(progn
	  (set-mark (pollen--tag-content-start tag))
	  (goto-char (pollen--tag-content-end tag))
	  (activate-mark))
      (signal 'scan-error '("No surrounding tag")))))

(defun pollen-change-surrounding-tag-name (next)
  "Change the tag name surrounding the content at point.

For example, if the point position is `|' and the name entered is \"bark\"

 Before: ◊tag{conten|ts}
 After : ◊bark{conten|ts}
"
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
  "Splits the content into two tags around the point.

For example, if the point position is `|'

 Before: ◊tag{conten|ts}
 After : ◊tag{conten}\n|◊tag{ts}
"
  (interactive)
  (let ((tag (pollen--tag-surrounding-point)))
    (if tag
	(progn
	  (push-mark (pollen--tag-closing-brace tag) nil)
	  (kill-region (point) (pollen--tag-content-end tag))
	  (goto-char (mark))
	  (insert (format "\n◊%s{%s}" (pollen--tag-name tag)
			  (s-trim (pop kill-ring))))
	  (goto-char (+ 1 (mark) ))
	  (pop-mark))
      (signal 'scan-error '("The point is not within a tag.  Unable to split.")))
    ))

(ert-deftest pollen--split-test ()
  (let ((texts
	 '(("◊tag{conten|ts}" "◊tag{conten}\n|◊tag{ts}")
	   ("◊tag{more ◊tagged{con|tents} inside}" "◊tag{more ◊tagged{con}\n|◊tagged{tents} inside}")
	   ("◊tag{m|ore ◊tagged{contents} inside}" "◊tag{m}\n|◊tag{ore ◊tagged{contents} inside}")
	   ("◊tag{more ◊tagged{contents} i|nside}" "◊tag{more ◊tagged{contents} i}\n|◊tag{nside}")
	   ;; TODO: Test errors
	   ;; ("◊ta|g{contents}" )
	   ;; ("◊ta|g{contents")
	   )))
    (--each texts
      (point-test-edit (car it)
		       (cadr it)
		       'pollen-split))))

(defun pollen-join ()
  "Join the tag surrounding the point with the next tag.

For example, if the point position is `|'

 Before: `◊tag{ha|s}◊tag{contents} `
 After : `◊tag{ha|s contents}`

There cannot be any non-whitespace characters between the tags. The
following tags cannot be joined:

  `◊tag{ha|s} some text between ◊tag{contents}`

"
  (interactive)
  (let ((tag (pollen--tag-surrounding-point)))
    (if tag
	(let* ((origin (point))
	       (next-tag (pollen--tag-after-tag tag))
	       (text-between
		(when next-tag
		  (buffer-substring (pollen--tag-closing-brace tag)
				    (pollen--tag-lozenge next-tag)))))
	  (if next-tag
	      (if (string-empty-p (s-trim text-between))
		  (progn
		    ;; Delete the next tag
		    (delete-region
		     (pollen--tag-closing-brace tag)
		     (pollen--tag-closing-brace next-tag))
		    ;; add the next tag contents onto the end of this one
		    (goto-char (pollen--tag-content-end tag))
		    (insert " ")
		    (insert (pollen--tag-content next-tag))
		    (goto-char origin))
		(signal 'scan-error
			`(,(format "There is text between this tag and the next [%s].  Unable to join."
				   (s-trim text))))
		)
	    (signal 'scan-error
		    '("There is no tag following this one.  Unable to join."))))
      (signal 'scan-error
	      '("The point is not within a tag.  Unable to join.")))))

(ert-deftest pollen--join-test ()
  (let ((texts '(("◊tag{ha|s}◊tag{contents}" "◊tag{ha|s contents}")
	 ("◊tag{ha|s}  ◊tag{contents}" "◊tag{ha|s contents}")
	 ("◊tag{ha|s}\n◊tag{contents}" "◊tag{ha|s contents}")
	 ;; TODO: Test errors
	 ;; ("◊ta|g{contents}" )
	 ;; ("◊ta|g{contents")
	 ;; ("◊tag{ha|s}more◊tag{contents}")
	 )))
    (--each texts
      (point-test-edit (car it)
		       (cadr it)
		       'pollen-join))))

(defun pollen-up-tag ()
  "Move the point out of the current tag to after its closing brace.

For example, if the point position is `|'

 Before: `◊foo{This is some conten|ts}`
 After : `◊foo{This is some contents}|`
"
  (interactive)
  (let ((tag (pollen--tag-surrounding-point)))
   (if tag
       (goto-char (pollen--tag-closing-brace tag))
       (signal 'scan-error
	       '("The point is not within a tag.  Unable to move up."))
       )))

(provide 'pollen-edit)
