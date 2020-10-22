;;; -*- lexical-binding: t; -*-
(require 'rx)
(require 'skeleton)
(require 'dash)
(require 'pollen-server)
(require 'pollen-edit)

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

(defconst pollen-font-lock-keywords
  (eval-when-compile
    ;; #lang
    `(
      (,(rx (group (group "#lang")
                   (1+ " ")
                   (group (1+ not-newline))))
       (2 font-lock-keyword-face nil t)
       (3 font-lock-variable-name-face nil t))
      (,(rx (group (group "◊")
		   (group (1+ (or word (syntax symbol))))
		   (group "{")
		   (group (1+ (not (any ?{ ?◊ ?}))))))
       (2 font-lock-keyword-face nil t)
       (3 font-lock-variable-name-face nil t)
       (5 font-lock-string-face nil t))
      (,(rx (group (1+ word)))
       (1 font-lock-string-face nil t))
      ))
  "Pollen mode keywords")

;; (setq font-lock-defaults (list (git-rebase-mode-font-lock-keywords) t t))
;; (font-lock-add-keywords 'pollen-markup-mode pollen-font-lock-keywords)


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
    map)
  "Keymap for pollen-markup-mode")

(defvar pollen-markup-mode-syntax-table
  (let ((table (make-syntax-table text-mode-syntax-table)))
    ;; The opening brace should match the closing brace
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
  (setq font-lock-defaults (list pollen-font-lock-keywords nil t)))

(provide 'pollen-markup-mode)
