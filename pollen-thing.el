;; Regular expressions for working with pollen

;; Define thing at point for:
;; contents {}
;; tag name
;; pollen-lozenge
;; Things:
;; 'pollen-opening-brace
;; 'pollen-lozenge
;; 'pollen-tag
;;
;; The tag content is found by finding the previous opening brace and using forward-sexp

(defconst pollen-lozenge-rx "◊")

(defconst pollen-tag-name-rx
  (rx (and "◊"
	   (group (one-or-more (not (any whitespace "◊" "{" "}"))))
	   "{"
	   ))
  "Regular expression that matches a pollen tag name.
The tag name corresponds to the only capture group")

(defconst pollen-opening-brace-rx
  (rx (and "◊"
	   (one-or-more (not (any whitespace "◊" "{" "}")))
	   (group "{")
	   ))
  "Regular expression that matches a pollen tag brace.
The brace corresponds to the only capture group")

(defun forward-pollen-lozenge (arg)
  "The forward-thing function for the pollen lozenge"
  (re-search-forward pollen-lozenge-rx nil t arg))

(defun forward-pollen-opening-brace (arg)
  (pollen--forward-regex pollen-opening-brace-rx 1 (natnump arg))
  )

(defun forward-pollen-tag (arg)
  "The forward-thing function for tagnames.

This moves the point (usually forward) to a tagname-related position (usually 
the next tagname).

ARG must be a positive or negative number.  The number itself is 
unimportant - we only consider it's sign.  Don't use this with a number of zero.

A tagname is the text of a tag, between the lozenge and the brace.
 - \"◊p{}\" has the tagname \"p\"
 - \"◊span[#:name \"Bruno\"]{}\" has the tagname \"span\".


If ARG is positive, this moves the point forward to a tagname related 
position (usually the next tagname). This has several scenarios:

 - The point is not in a tag, but there is a tag somewhere after it.
   It is moved to the end of the next tag name.

 - The point is not in a tag, and there isn't a tag after it.
   It remains where it is.

 - The point is within a tag, at the lozenge or somewhere within the tag name.
   It is moved to the end of the current tag name.

 - The point is within a tag, just after the tag name.
   It is moved to the end of the next tag name.

If ARG is negative, this moves the point backward to the beginning of the tag
 name. This has the scenarios:

 - The point is not in a tag, but there is a tag somewhere before it.
   It is moved to the start of the tag name before it.

 - The point is not in a tag, and there isn't a tag before it.
   It remains where it is.

 - The point is within a tag, at the { or somewhere within the tag name.
   It is moved to the start of the current tag name.

 - The point is within a tag, just before the tag name
   It is moved to the start of the tag name before it. "
  (pollen--forward-regex pollen-tag-name-rx 1 (natnump arg))
  )

(defun pollen--forward-regex (regex group-n is-forward)
"Moves the point to the next string matching REGEX.

If IS-FORWARD is `t', the match is after the point. If it is `nil', the match is
 behind it.

If IS-FORWARD is `t', the point is moved to the end of the GROUP-N th group of 
the match. If it is `nil', the point is moved to the group start.

The point may already be within the GROUP-N th group of a match.  In this case, 
it is moved to the edge of the group."
  ;; "Behind" refers to the direction opposite to the point's movement
  ;;   If is-forward is t, "behind" means towards the beginning of the buffer
  ;;   If is-forward is nil, "behind" means towards the end of the buffer
  ;; "in-front" and "front" refer to the direction or side towards the point's
  ;; movement.  This is the opposite of "behind".
  (let* ((starting-point (point))
	 ;; The position of the front of a regular expression match.
	 (match-front-function (if is-forward 'match-end 'match-beginning))
	 ;; Whether a point is in front of another
	 (in-front-p (if is-forward '>= '<=))
	 ;; The farthest point behind
	 (farthest-behind (if is-forward (point-min) (point-max)))

	 ;; The search functions move the point to the next match.
	 ;; They return nil if none is found.
	 (direction (if is-forward 1 -1))
	 (search-in-front-function (lambda ()
			    (re-search-forward regex nil t direction)))
	 (search-behind-function (lambda ()
			    (re-search-forward regex nil t (- direction))))
	 (end-point
	  (save-excursion
	    ;; First search behind for the nearest match.
	    ;; We need to do this to pick up on whether the starting
	    ;; point is within a match.
	    (if (funcall search-behind-function)
		;; If we found a match, move the point in front of it, to the
		;; character closest to the starting point.
		;; This ensures we skip over this match in the following search.
		(goto-char (funcall match-front-function 0))
	      ;; If none is found, move the point as far behind as we can.
	      ;; We need to do this to move the point outside of a match, as it
	      ;; might be within one.
	      (goto-char farthest-behind))
	    ;; From there, search in front for the next match.
	    ;; If the starting point was within a match, that match
	    ;; will be found.
	    ;; If not, this finds the match in front of the starting point.
	    (when (funcall search-in-front-function)
	      ;; If we found a match, find the front of the match's group.
	      ;; E.g. when searching forwards with the regex "f(ol)d", where
	      ;; "ol" is the group, this would find the position between "l"
	      ;; and "d".
	      (let ((front-of-group (funcall match-front-function group-n)))
		;; If the starting point is at or in front of the group, then
		;; it must have been within this match, but in front of the
		;; group. We want to move on to the next match.
		(if (funcall in-front-p starting-point front-of-group)
		    (when (funcall search-in-front-function)
		      (funcall match-front-function group-n)
		      ;; If there was no next match, the starting point is after
		      ;; the last match in the buffer.  Return nil.
		      )
		  ;; If not, this is the match we want to move to.
		  ;; Return the position of the front of it's group.
		  front-of-group
		  ))
	      ;; If nothing is found, there are no more matches in the buffer.
	      ;; Return nil.
	      )
	    )
	  ))
    (when end-point (goto-char end-point))
    ))

;; TODO: Can we make a symbol and set it to this value ?
(defun forward-thing-pollen-tag ()
  (forward-thing 'pollen-tag 1))

(defun backward-thing-pollen-tag ()
  (forward-thing 'pollen-tag -1))

(defun forward-thing-pollen-opening-brace ()
  (forward-thing 'pollen-opening-brace 1))

(defun backward-thing-pollen-opening-brace ()
  (forward-thing 'pollen-opening-brace -1))

(ert-deftest forward-pollen-tag-forwards ()
  "`(forward-thing 'pollen-tag 1)' should move the point correctly."
  (let ((texts
	 '("The poi①nt ◊should②{move here} if there's no tag before it"
	   "The ◊point{should} mov①e ◊forwards②{with} if there's a tag before it"
	   "The point ◊shou①ld②{move to} the end if there's no tag after it"
	   "The point ◊shou①ld②{move to} the ◊tag{end} if there's a tag after it"
	   "The point ◊should①{move to} the ◊next②{tag} if it's after the tag name"
	   "The point ◊①should②{move to} the end if it's before the tag name"
	   )))
    (--each texts (point-test-motion 'forward-thing-pollen-tag it))))

(ert-deftest forward-pollen-tag-backwards ()
  "`(forward-thing 'pollen-tag -1)' should move the point correctly."
  (let ((texts
	 '("The point ◊②should{move here} if th①ere's no tag after it"
	   "The ◊②point{should} mo①ve ◊backwards{if} there's a tag after it"
	   "The point ◊②shou①ld{move to} the start if there's no tag before it"
	   "The point ◊should{move to} the ◊②sta①rt{if} if there's a tag before it"
	   "The point ◊②should{move to} the ◊①previous{tag} if it's before the tag name"
	   "The point ◊②should①{move to} the end if it's after the tag name")))
    (--each texts (point-test-motion 'backward-thing-pollen-tag it))))

(ert-deftest forward-pollen-opening-brace-forwards ()
  "`(forward-thing 'pollen-opening-brace 1)' should move the point correctly."
  (let ((texts
	 '("The po①int ◊should{②move here} if it's before a tag"
	   "The ◊point{should mo①ve ◊here{②if it's} within} the content"
	   "The point ◊shou①ld{②move here} if it's within a tagname"
	   "Th①e {point} ◊should{②not move to} a brace not part of a tag"
	   )))
    (--each texts (point-test-motion 'forward-thing-pollen-opening-brace it))))

(ert-deftest forward-pollen-opening-brace-backwards ()
  "`(forward-thing 'pollen-tag -1)' should move the point correctly."
  (let ((texts
	 '("The point ◊should②①{move here} if it's afte|r a tag"
	   "The ◊point{should move ◊here②{if it's} w①ithin} the content"
	   "The point ◊should②{move ◊he①re{if it's within}} a tagname"
	   "The point ◊should②{not move to} a {brace} not p①art of a tag"
	   )))
    (--each texts (point-test-motion 'backward-thing-pollen-opening-brace it))))
