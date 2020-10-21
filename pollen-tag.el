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
    ;; If there is a tag trailing before the point, move behind it
    (when-let* ((bounds (bounds-of-thing-at-point 'pollen-tag))
		(before-lozenge (car bounds))
		(after-open-brace (cdr bounds))
		(should-move (and (not (= before-lozenge (point)))
				  (not (= after-open-brace (point))))))
      (forward-thing 'pollen-tag -1))
    ;; Move behind the tag
    ;; If the point did not move, there is no tag before.  Return nil.
    (when (pollen--forward-thing-p 'pollen-tag -1)
      (let* ((bounds (bounds-of-thing-at-point 'pollen-tag))
	    (name-bounds (cons (+ 1 (car bounds)) (- (cdr bounds) 1)))
	    (name (buffer-substring (car name-bounds) (cdr name-bounds)))
	    ;; The content starts in front of the opening brace
	    (content-start (cdr bounds)))
	;; Try and find the closing brace
	(condition-case nil
	    (progn
	      ;; Move to just before the opening brace
	      (goto-char (- (cdr bounds) 1))
	      (forward-sexp)
	      (let* ((content-end (- (point) 1))
		     (content (buffer-substring-no-properties
			       content-start
			       content-end)))
		(make-pollen--tag
		 :name name
		 :name-bounds name-bounds
		 :content content
		 :content-bounds (cons content-start content-end))))
	  ;; There was no closing brace. Return nil.
	  (scan-error nil))))))


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
      (message "The point is at %s" (point-test--around (point)))
      (pollen--tag-surrounding-point-go origin)
      ))
  )

(ert-deftest pollen--tag-before-point ()
  (let ((texts
	 '(("◊tag{conten|ts}" "tag" "contents")
	   ("◊tag{|contents}" "tag" "contents")
	   ("◊tag{more ◊tagged{con|tents} inside}" "tagged" "contents")
	   ("◊tag{m|ore ◊tagged{contents} inside}" "tag" "more ◊tagged{contents} inside")
	   ("◊tag{more ◊tagged{contents} i|nside}" "tagged" "contents")
	   ("◊tag{◊m{ore} ◊tagg|ed{contents} inside}" "m" "ore")
	   ("◊tag{more |◊tagged{contents} inside}" "tag" "more ◊tagged{contents} inside")
	   ("◊ta|g{contents}" () ())
	   ("◊ta|g{contents" () ())
	   )))
    (--each texts (progn
		    (point-test-result (car it)
				       (cadr it)
				       (lambda ()
					 (when-let ((tag (pollen--tag-before-point)))
					   (pollen--tag-name tag)))
				       'pollen--tag-name-before-point)
		    (point-test-result (car it)
				       (caddr it)
				       (lambda ()
					 (when-let ((tag (pollen--tag-before-point)))
					   (pollen--tag-content tag)))
				       'pollen--tag-content-before-point)))))

(ert-deftest pollen--tag-surrounding-point ()
  (let ((texts
	 '(("◊tag{conten|ts}" "tag" "contents")
	   ("◊tag{|contents}" "tag" "contents")
	   ("◊tag{more ◊tagged{con|tents} inside}" "tagged" "contents")
	   ("◊tag{m|ore ◊tagged{contents} inside}" "tag" "more ◊tagged{contents} inside")
	   ("◊tag{more ◊tagged{contents} i|nside}" "tag" "more ◊tagged{contents} inside")
	   ("◊tag{◊m{ore} ◊tagged{contents} i|nside}" "tag" "◊m{ore} ◊tagged{contents} inside")
	   ("◊ta|g{contents}" () ())
	   ("◊ta|g{contents" () ()))))
    (--each texts (progn
		    (point-test-result (car it)
				       (cadr it)
				       (lambda ()
					 (when-let ((tag (pollen--tag-surrounding-point)))
					   (pollen--tag-name tag)))
				       'pollen--tag-name-surrounding-point)
		    (point-test-result (car it)
				       (caddr it)
				       (lambda ()
					 (when-let ((tag (pollen--tag-surrounding-point)))
					   (pollen--tag-content tag)))
				       'pollen--tag-content-surrounding-point
				       )))))

(defun pollen--tag-after-tag (tag)
  "The tag after TAG"
  (save-excursion
    (goto-char (pollen--tag-closing-brace tag))
    (when (pollen--forward-thing-p 'pollen-tag 1)
      (pollen--tag-before-point))))

(ert-deftest pollen--tag-after-tag-test ()
  (let ((texts
	 '(("◊tag{conten|ts}◊after{}" "after")
	   ("◊tag{conten|ts} ◊after{}" "after")
	   ("◊tag{conten|ts}\n ◊after{}" "after")
	   ("◊tag{conten|ts} text between ◊after{more contents}" "after")
	   ("◊tag{m|ore ◊tagged{contents} inside}◊after{}" "after")
	   ("◊tag{more ◊tagged{contents} i|nsi◊{de}}◊after{}" "after")
	   ("◊tag{c|ontents}" ()))))
    (--each texts (point-test-result (car it)
				     (cadr it)
				     (lambda ()
				       (when-let
					   ((tag (pollen--tag-after-tag
						  (pollen--tag-surrounding-point))))
					 (pollen--tag-name tag)))
				     'pollen--tag-after-tag-surrounding-point))))

(defun pollen--forward-thing-p (thing arg)
  (let ((start (point)))
    (forward-thing thing arg)
    (not (equal start (point)))))

(ert-deftest pollen--thing-at-point-pollen-tag-test ()
  (let ((texts '(("◊ta|g{" "◊tag{" )
		 ("|◊tag{" "◊tag{" )
		 ("◊tag{|" "◊tag{" ))))
    (--each texts (progn
		    (point-test-result (car it)
				       (cadr it)
				       (lambda () (thing-at-point 'pollen-tag))
				       'pollen--tag-at-point)))))


;; TODO: Should ert tests end with test?
