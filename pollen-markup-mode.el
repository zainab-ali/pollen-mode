;;; -*- lexical-binding: t; -*-
;; Lexical binding is described in Info node `(elisp) Lexical Binding'

(require 'rx)

(require 'skeleton)
(require 'dash)
(require 'pollen-server)
(require 'pollen-edit)
(require 'pollen-face)

(defvar pollen-abbrevs
  '(("pp"  . p)
    ("ph"  . headline2)
    ("pn"  . note)
    ("pk"  . keyword)
    ("pd"  . definition)
    ("pb"  . buffer)
    ("pc"  . command)
    ("pci"  . code-inline)
    ("pf"  . function)
    ("pky"  . key)
    ("pkb"  . keybinding))
  "Tagnames")

(define-skeleton pollen-skeleton-command
  "Pollen command"
  "Tag: "
  "◊" str "{" _ "}")

(defmacro pollen-skeleton-tag (tag)
  `(progn
     (defun ,(intern (concat "pollen-skeleton-command-" tag)) ()
       (pollen-skeleton-command ,tag)
       )
     (put (quote ,(intern (concat "pollen-skeleton-command-" tag)))  'no-self-insert t)
     )
  )

(defun pollen--skeleton-abbrev (abbrev tag-name)
  "Creates an entry for the abbrev table"
  (let ((command (intern (concat "pollen-skeleton-command-" (symbol-name tag-name)))))
    (fset command (lambda ()
		    (pollen-skeleton-command (symbol-name tag-name))))
    (put command 'no-self-insert t)
    (list abbrev "" command)))

(define-abbrev-table 'pollen-markup-mode-abbrev-table
  (cons
   (list "psc" "" 'pollen-skeleton-command)
   (--each pollen-abbrevs (pollen--skeleton-abbrev (car it) (cdr it)))))


(defvar pollen-markup-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c j") 'pollen-join)
    (define-key map (kbd "C-c s") 'pollen-split)
    (define-key map (kbd "C-c c") 'pollen-change-surrounding-tag-name)
    (define-key map (kbd "C-c d") 'pollen-delete-surrounding-tag)
    (define-key map (kbd "C-c ]") 'pollen-up-tag)
    map)
  "Keymap for pollen-markup-mode")

(defvar pollen-markup-mode-syntax-table
  (let ((table (make-syntax-table text-mode-syntax-table)))
    ;; The opening brace should match the closing brace
    ;; TODO: This doesn’t seem to work with electric-pair-mode. Why?
    (modify-syntax-entry ?{ "(}")
    (modify-syntax-entry ?} "){")
    ;; '-' is a symbol constituent
    (modify-syntax-entry ?- "_")
    table)
  )

;;;###autoload
(define-derived-mode pollen-markup-mode text-mode "Pollen markup"
  "Major mode for editing pollen markup."
  :syntax-table pollen-markup-mode-syntax-table
  :abbrev-table pollen-markup-mode-abbrev-table
  :after-hook (font-lock-ensure)
  :keymap pollen-markup-mode-map
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
