;; Utils for testing the point

;; Point insertion
;; (defun insert-point-start () (interactive) (insert "①"))
;; (defun insert-point-end () (interactive) (insert "②"))

;; (local-set-key (kbd "C-c 1") 'insert-point-start)
;; (local-set-key (kbd "C-c 2") 'insert-point-end)

(defun point-test-motion (marked-text function &optional lambda-name)
  "Verifies that calling FUNCTION moves the point from ① to ② within the MARKED-TEXT.
The MARKED-TEXT must contain a single ① to mark the start point and a single ② to mark the end point.

(point-test-motion \"Hello wo②r①ld!\" 'backward-char) returns nil
(point-test-motion \"Hello wor①l②d!\" 'backward-char) signals an error."
  (let ((name (point-test--name function lambda-name))
	(start (point-test--between (point-test--s-remove marked-text "②") "①"))
	(end (point-test--between (point-test--s-remove marked-text "①") "②"))
	(text (point-test--s-remove (point-test--s-remove marked-text "①") "②")))
    (if name
	(with-temp-buffer
	       (insert text)
	       (goto-char start)
	       (funcall function)
	       (unless (= (point) end)
		 (ert-fail
		  (s-join "\n"
			  (list
			   "\nPoint was moved to the wrong position.\n"
			   (format "function          : %s" name)
			   (format "text              : [%s]" text)
			   (format "start point       : %s" (point-test--around start))
			   (format "expected end point: %s" (point-test--around end))
			   (format "actual end point  : %s" (point-test--around (point))))))))
      (point-test--name-error 'point-test-motion))))

(defun point-test-result (marked-text result function &optional lambda-name)
  "Move the point to the | in the MARKED-TEXT and run FUNCTION.
Verify that it returns RESULT."
  (let ((name (point-test--name function lambda-name))
	(point (point-test--between marked-text "|"))
	(text (point-test--s-remove marked-text "|")))
    (if name
	(with-temp-buffer
	  (insert text)
	  (goto-char point)
	  (let ((actual-result (funcall function)))
	    (unless (equal actual-result result)
	      (ert-fail
	       (s-join "\n"
		       (list
			"\nResult at point was wrong.\n"
			(format "function       : %s" name)
			(format "text           : [%s]" text)
			(format "point          : %s" (point-test--around point))
			(format "expected result: %S" result)
			(format "actual result  : %S" actual-result)))))))
      (point-test--name-error 'point-test-result))))

(defun point-test-edit (start-marked-text end-marked-text function &optional lambda-name)
  "Move the point to the | in the START-MARKED-TEXT and run FUNCTION.
Verify that it is at | in the END-MARKED-TEXT."
  (let (
	(name (point-test--name function lambda-name))
	(start-point (point-test--between start-marked-text "|"))
	     (start-text (point-test--s-remove start-marked-text "|"))
	     (end-point (point-test--between end-marked-text "|"))
	     (end-text (point-test--s-remove end-marked-text "|")))
    (if name (with-temp-buffer
	       (insert start-text)
	       (goto-char start-point)
	       (funcall function)
	       (let ((text (buffer-substring-no-properties (point-min)
							   (point-max))))
		 (unless (and (equal text end-text) (equal (point) end-point))
		   (ert-fail
		    (s-join "\n"
			    (list
			     "\Edit at point was wrong.\n"
			     (format "function          : %s" name)
			     (format "start text        : [%s]" start-text)
			     (format "expected end text : [%s]" end-text)
			     (format "actual end text   : [%s]" text)
			     (format "start point       : %s" (point-test--around-text start-text start-point))
			     (format "actual end point  : %s" (point-test--around (point)))
			     (format "expected end point: %s" (point-test--around-text end-text end-point))))))))
      (point-test--name-error 'point-test-edit))))

(defun point-test--s-remove (text char)
  (s-replace char "" text))

(defun point-test--between (text char)
  "The poisition of the point before CHAR in TEXT."
  (let* ((parts (s-split char text))
	(num-parts (length parts)))
    (if (= 2 num-parts)
	(+ 1 (length (car parts)))
	(ert-fail (format "Text must contain a single %s character. It contained %s. Text:\n [%s]" char (- num-parts 1) text)))))

(defun point-test--extract (marked-text regex)
  "Extract the actual text, and points from a text with points marked as \"|\"."
  (let ((match (s-match regex marked-text)))
    (if match
	(let* (full-text
	       (texts (cdr match))
	       (points (with-temp-buffer
			 (--reduce-from
			  (progn (insert it)
				 (cons (point) acc))
			  nil
			  (-remove-at (- (length texts) 1) texts))
			 )))
	  (cons (apply 'concat texts)
		(reverse points))))))

(defun point-test--around-text (text point)
  "Returns the text around POINT in TEXT. "
  (with-temp-buffer
    (insert text)
    (point-test--around point)))

(defun point-test--around (point)
  "Returns the text around POINT.

The first start contains the threee characters before POINT, or up to three
if there are fewer.
The end contains the three characters after it.
The point itself is denoted by \"|\".
"
  (let ((start (max (point-min) (- point 3)))
	(end (min (point-max) (+ point 3))))
    (concat (buffer-substring-no-properties start point) "|"
	  (buffer-substring-no-properties end point))))

(ert-deftest point-test--around-test ()
  "The test helper `point-test--around' should return the characters around the
 point"
  (with-temp-buffer
    (insert "Some text")
    (should (equal (point-test--around (point-min)) "|Som"))
    (should (equal (point-test--around 5) "ome| te"))
    (should (equal (point-test--around (point-max)) "ext|"))))

(ert-deftest point-test--around-empty ()
  "The test helper `point-test--around' should return empty strings in an empty
 buffer"
  (with-temp-buffer
    (should (equal (point-test--around (point-min)) "|"))
    (should (equal (point-test--around (point-max)) "|"))))

(defun point-test--name (function lambda-name)
  (or lambda-name (when (symbolp function) (symbol-name function))))

(defun point-test--name-error (function)
  (ert-fail (format "%S: A lambda-name is required when using a lambda" function)))
