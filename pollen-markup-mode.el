;;; -*- lexical-binding: t; -*-
;; Lexical binding is described in Info node `(elisp) Lexical Binding'

(require 'dash)
(require 'pollen-server)
(require 'pollen-edit)
(require 'pollen-face)

;; The keymap defines key bindings, as described in Info node `(elisp) Keymaps'.
;; There are no keybindings by default, but we create a keymap so that users can
;; add bindings to it themselves.
(defvar pollen-markup-mode-map (make-sparse-keymap)
  "Keymap for pollen-markup-mode")

;; Defines the role of each character, as described in
;; Info node `(elisp) Syntax Tables'.
;; Pollen tag contents are treated as text, so we can inherit from
;; `text-mode-syntax-table'.
;;
;; Pollen has the following "special" characters:
;; - The lozenge `?◊' should be treated as punctuation
;;   This is done by `text-mode-syntax-table', so no entry is added
;; - The braces `?{' and `?}' should be treated as parentheses.
;;   This is also done by `text-mode-syntax-table'.
;; - The dash `?-' may be used in tag names e.g. "◊foo-bar{". This should be a
;;   symbol constituent to ensure that `foo-bar' is treated as whole word
;;   instead of two separate words.
;;   We need to add an entry in the syntax table for this.
(defvar pollen-markup-mode-syntax-table
  (let ((table (make-syntax-table text-mode-syntax-table)))
    ;; The dash character `?-' is a symbol constituent. It has no corresponding
    ;; paired character (as an example, `?{' and `?}' are paired together). It
    ;; also has nothing to do with comments (pollen has no comments).
    ;; Thus, it's entry is `"_"'.
    (modify-syntax-entry ?- "_")
    table)
  "Syntax table for pollen-markup-mode"
  )

;; `autoload' registers the mode, but defers loading the entire file as
;; described in Info node `(elisp) Autoload'. If a user seldomly writes pollen,
;; they may not use the mode at all in an Emacs sitting. Loading the code every
;; time they start Emacs would be slow and pointless. `autoloading' avoids this.
;;;###autoload
(define-derived-mode
  pollen-markup-mode
  ;; Editing pollen is like editing text, so inherit from `text-mode'
  text-mode
  "Pollen markup"
  "Major mode for editing pollen markup."
  ;; The syntax table defines the role (the syntax class) of each character. For
  ;; example, it defines what counts as punctuation, what counts as a comment
  ;; start, and what is treated as a parenthesis.
  :syntax-table pollen-markup-mode-syntax-table
  ;; After pollen-markup-mode has been enabled in a buffer, fontify the entire
  ;; buffer
  :after-hook (font-lock-ensure)
  ;; Define the keybindings
  :keymap pollen-markup-mode-map
  ;; Before running `font-lock-mode-hook', set the `font-lock-defaults'.
  (setq font-lock-defaults
	(list
	 ;; The symbol to use for `font-lock-keywords'.
	 pollen-font-lock-keywords
	 ;; Corresponds to `font-lock-keywords-only'. This indicates whether or
	 ;; not font lock should fontify strings and comments. Strings and
	 ;; comments are identified via the syntax table, as described in
	 ;; Info node `(elisp) Syntactic Font Lock'.
	 ;; We don't care (there are no strings or comments in pollen), so set
	 ;; it to `t' to disable string/comment fontification.
	 t
	 ;; Corresponds to `font-lock-keywords-case-fold-search'. As this is `t'
	 ;; the regular expression match is case-insensitive. case isn't
	 ;; important for pollen syntax, so we set it to `t'.
	 t
	 )))

(provide 'pollen-markup-mode)
