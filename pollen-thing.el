(require 'rx)
(require 'dash)

(defconst pollen-lozenge-rx "◊")

(defun forward-pollen-tag (arg)
  "The forward-thing function for tags

This moves the point (usually forward) to a tag-related position (usually
the next tag).  Unless you're well versed in forward-THING functions, this is
probably not what you want to use.

ARG must be a positive or negative number.  The number itself is
unimportant - we only consider it's sign.  Don't use this with a number of zero.

A tag thing is the lozenge, the text of a tag, and the opening brace.
 - \"◊p{hello}\" has the tag \"◊p{\"
 - \"◊span[#:name \"Bruno\"]{}\" has the tag \"◊span[#:name \"Bruno\"]{\".


If ARG is positive, this moves the point forward to a tag related
position (usually the next tag). This has several scenarios:

 - The point is not in a tag, but there is a tag somewhere after it.
   It is moved to the end of the next tag.

 - The point is not in a tag, and there isn't a tag after it.
   It remains where it is.

 - The point is within a tag, at the lozenge or somewhere within the tag name.
   It is moved to the end of the current tag.

 - The point is just after the opening brace of a tag.
   It is moved to the end of the next tag.

If ARG is negative, this moves the point backward to the beginning of the tag.
 This has the scenarios:

 - The point is not in a tag, but there is a tag somewhere before it.
   It is moved to the start of the tag before it.

 - The point is not in a tag, and there isn't a tag before it.
   It remains where it is.

 - The point is within a tag, after the { or somewhere within the tag name.
   It is moved to the start of the current tag.

 - The point is within a tag, just before the lozenge
   It is moved to the start of the tag before it. "
  (if-let* ((bounds
	     (or (pollen--look-around
		  (rx (and "◊" (zero-or-more (not (any whitespace "◊" "{" "}")))))
		  (rx (and (one-or-more (not (any whitespace "◊" "{" "}"))) "{")))
		 (pollen--look-around
		  (rx (and "◊" (one-or-more (not (any whitespace "◊" "{" "}")))))
		  (rx (and (zero-or-more (not (any whitespace "◊" "{" "}"))) "{")))) )
	    (point (if (natnump arg) (cdr bounds) (car bounds))))
      (goto-char point)
    (re-search-forward (rx (and "◊" (one-or-more (not (any whitespace "◊" "{" "}")))
				"{"))
		       nil t arg) ))

(ert-deftest forward-pollen-tag-forwards-test ()
  "`(forward-thing 'pollen-tag 1)' should move the point correctly."
  (let ((texts
	 '("The poi①nt ◊should{②move here} if there's no tag before it"
	   "The ◊point{should} mov①e ◊forwards{②with} if there's a tag before it"
	   "The point ◊shou①ld{②move to} the end if there's no tag after it"
	   "The point ◊shou①ld{②move to} the ◊tag{end} if there's a tag after it"
	   "The point ◊should{①move to} the ◊next{②tag} if it's after the tag"
	   "The point ◊①should{②move to} the end if it's before the tag")))
    (--each texts (point-test-motion it (lambda ()
					  (forward-thing 'pollen-tag 1))
				     'forward-pollen-tag-forwards))))

(ert-deftest forward-pollen-tag-backwards-test ()
  "`(forward-thing 'pollen-tag -1)' should move the point correctly."
  (let ((texts
	 '("The point ②◊should{move here} if th①ere's no tag after it"
	   "The ②◊point{should} mo①ve ◊backwards{if} there's a tag after it"
	   "The point ②◊shou①ld{move to} the start if there's no tag before it"
	   "The point ◊should{move to} the ②◊sta①rt{if} if there's a tag before it"
	   "The point ②◊should{move to} the ①◊previous{tag} if it's before the tag"
	   "The point ②◊should{①move to} the end if it's after the tag")))
    (--each texts (point-test-motion it (lambda () (forward-thing 'pollen-tag -1))
				     'forward-pollen-tag-backwards))))

(defun pollen--look-around (regex-back regex-forward)
  (when-let* ((forward (looking-at regex-forward))
	      (match-end (match-end 0)))
    (when-let* ((back (looking-back regex-back))
		(match-beginning (match-beginning 0)))
      (cons match-beginning match-end))))

(provide 'pollen-thing)
