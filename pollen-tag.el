(defstruct pollen--tag
  (name)
  (name-bounds)
  (content)
  (content-bounds))

(defun pollen--tag-lozenge (tag)
  "The position before the lozenge"
  (- (car (pollen--tag-name-bounds tag)) 1))

(defun pollen--tag-name-start (tag)
  "The position before the start of the name"
  (car (pollen--tag-name-bounds tag)))

(defun pollen--tag-name-end (tag)
  "The position after the end of the name"
  (cdr (pollen--tag-name-bounds tag)))

(defun pollen--tag-content-start (tag)
  "The position before the start of the content"
  (car (pollen--tag-content-bounds tag)))

(defun pollen--tag-content-end (tag)
  "The position after the end of the content"
  (cdr (pollen--tag-content-bounds tag)))

(defun pollen--tag-opening-brace (tag)
  "The position before the opening brace {."
  (- (pollen--tag-content-start tag) 1))

(defun pollen--tag-closing-brace (tag)
  "The position after the closing brace }"
  (+ (pollen--tag-content-end tag) 1))

(defun pollen--tag-before-point ()
  "The tag with an opening brace before the point.  Returns nil if there is none."
  (save-excursion
    (let ((start (point))
	  (name nil)
	  (name-bounds nil)
	  (content nil)
	  (content-start nil)
	  (content-end nil))
      ;; Move behind the opening brace
      (forward-thing 'pollen-opening-brace -1)
      ;; If the point did not move, there is no opening brace.  Return nil.
      (unless (equal (point) start)
	(setq name (thing-at-point 'pollen-tag))
	(setq name-bounds (bounds-of-thing-at-point 'pollen-tag))
	;; The content starts in front of the opening brace
	(setq content-start (+ 1 (point)))
	;; Try and find the closing brace
	(condition-case nil
	    (progn (forward-sexp)
		   (setq content-end (- (point) 1))
		   (setq content (buffer-substring-no-properties content-start content-end))
		   (make-pollen--tag
		      :name name
		      :name-bounds name-bounds
		      :content content
		      :content-bounds (cons content-start content-end)))
	  ;; There was no closing brace. Return nil.
	  (scan-error nil))
	))))

(defun pollen--tag-surrounding-point ()
  "The tag containing the point.  Returns nil if there is none."
   (save-excursion (pollen--tag-surrounding-point-go (point))) 
  )


(defun pollen--tag-surrounding-point-go (origin)
  "The tag containing the origin.  Returns nil if there is none."
  ;; Get the tag opening before the point.  Return nil if there is none.
  (when-let ((tag (pollen--tag-before-point)))
    ;; If the tag ends after the origin, it must contain it.  Return the tag.
    (if (> (pollen--tag-closing-brace tag) origin)
	tag
      ;; If not, move before the tag and look again.
      (goto-char (pollen--tag-lozenge tag))
      (pollen--tag-surrounding-point-go origin)
      ))
  )

;; TODO: Make local
(defun pollen--tag-name-surrounding-point ()
  (when-let ((tag (pollen--tag-surrounding-point)))
    (pollen--tag-name tag)))
(defun pollen--tag-content-surrounding-point ()
  (when-let ((tag (pollen--tag-surrounding-point)))
    (pollen--tag-content tag)))

(ert-deftest pollen--tag-surrounding-point ()
  (let ((texts
	 '(("◊tag{conten|ts}" "tag" "contents")
	   ("◊tag{more ◊tagged{con|tents} inside}" "tagged" "contents")
	   ("◊tag{m|ore ◊tagged{contents} inside}" "tag" "more ◊tagged{contents} inside")
	   ("◊tag{more ◊tagged{contents} i|nside}" "tag" "more ◊tagged{contents} inside")
	   ("◊tag{◊m{ore} ◊tagged{contents} i|nside}" "tag" "◊m{ore} ◊tagged{contents} inside")
	   ("◊ta|g{contents}" () ())
	   ("◊ta|g{contents" () ())
	   )))
    (--each texts (progn
		    (point-test-result (car it)
				       'pollen--tag-name-surrounding-point
				       (cadr it))
		    (point-test-result (car it)
				       'pollen--tag-content-surrounding-point
				       (caddr it))))))
