;;; -*- lexical-binding: t; -*-
;; Lexical binding is described in `(info "(elisp) Lexical Binding")'

;; Rx notation is used to construct regular expressions, as described in 
;; `(info "(elisp) Rx Notation")'
(require 'rx)


;; Configure how pollen syntax is displayed using faces, as described in
;; `(info "(elisp) Faces")'.
;; We don't care about which terminal (cli) the user opened Emacs in, so DISPLAY
;; is always `t'.
;; Since choosing colours (and other properties) is a tricky design problem, we
;; defer to pre-defined properties by inheriting from font lock's faces with the
;; `:inherit' attribute.
(defface pollen-tagname-face
  '((t :inherit font-lock-constant-face))
  "The face for the pollen tag name: \"foo\" in \"◊foo{\"")

(defface pollen-contents-face
  '((t :inherit default))
  "The face for the pollen tag contents: \"bar\" in \"◊foo{bar}\"")

(defface pollen-lozenge-face
  '((t :inherit font-lock-keyword-face))
  "The face for the pollen lozenge: \"◊\" in \"◊foo{bar}\"")

(defface pollen-attribute-name-face
  '((t :inherit font-lock-keyword-face))
  "The face for #:baz in `◊foo[#:baz \"cow\"]{bar}'")

(defface pollen-attribute-value-face
  '((t :inherit font-lock-string-face))
  "The face for `\"cow\"' in `◊foo[#:baz \"cow\"]{bar}'")

(defface pollen-hashlang-face
  '((t :inherit font-lock-keyword-face))
  "The face for \"#lang\" in \"#lang pollen\"")

(defface pollen-lang-face
  '((t :inherit font-lock-constant-face))
  "The face for \"pollen\" in \"#lang pollen\"")


;; The font lock keywords determine the face for a given piece of pollen syntax
;; by testing it with a regular expression matcher. The test and colouring is
;; done by `font-lock-mode'.
(defconst pollen-font-lock-keywords
  (eval-when-compile
    `(
      ;; All keywords are of the form
      ;; (matcher highlighters…)
      ;; where each highlighter is of the form
      ;; (subexp facespec override laxmatch)
      ;; as described in `(info "(elisp) Search-based Fontification")'
      ;;
      ;; We never want to override existing fontification, so OVERRIDE is always
      ;; `nil'.
      ;; We never want to raise an error if a regular expression doesn't match,
      ;; so LAXMATCH is always `t' .
      ;;
      ;; The regular expressions for the matcher are composed using the rx
      ;; notation as described in `(info "(elisp) Rx Notation")'

      ;; Matches the Racket lang line "#lang pollen"
      (,(rx (group "#lang")
            (1+ " ")
            (group (1+ not-newline)))
       ;; Regex capture group 1 corresponds to the "#lang"
       (1 'pollen-hashlang-face nil t)
       ;; Group 2 corresponds to the "pollen"
       (2 'pollen-lang-face nil t))
      ;; Matches the tag name "◊foo{" or "◊foo[#:bar "baz"]{"
      (,(rx (group "◊")
	    (group (1+ (or word (syntax symbol))))
	    (optional "["
		      (1+ "#:" (1+ (or word (syntax symbol)))
			  blank
			  ?\" (1+ (not ?\")) ?\"
			  (optional blank))
		      "]")
	    "{")
       ;; Group 1 corresponds to the "◊"
       (1 'pollen-lozenge-face nil t)
       ;; Group 2 corresponds to the name ("foo" in "◊foo{")
       (2 'pollen-tagname-face nil t))
      ;; Matches the attribute name-value pair "#:bar "baz""
      (,(rx (group "#:" (1+ (or word (syntax symbol))))
	    blank
	    (group ?\" (1+ (not ?\")) ?\"))
       ;; Group 1 corresponds to the "#:bar"
       (1 'pollen-attribute-name-face nil t)
       ;; Group 2 corresponds to the ""baz""
       (2 'pollen-attribute-value-face nil t))
      ;; Corresponds to words and whitespace that make up the tag contents.
      (,(rx (1+ (not (any ?{ ?◊ ?} ?\n))))
       ;; The entire capture contents (group 0).
       (0 'pollen-contents-face nil t))
      ))
  "Pollen mode font lock keywords. 
These are set as defaults for `font-lock-mode' so it can colour certain syntax.
")

(provide 'pollen-face)
