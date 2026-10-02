;;; my-notes.el ---  -*- lexical-binding: t; -*-
;;; -----1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2------
;;; Commentary:
;;; Code:

(require 'denote)
(require 'consult-notes)

(defun my/denote-format-keywords-for-md-front-matter (keywords)
  "Custom KEYWORDS formatter for keystrokecountdown.com markdown files.
The default Markdown keyword formatter puts each keyword in double-quotes,
separates them with a \", \" and surrounds the result with square brackets.
Here, we just separate them by a comma."
  (format "%s" (mapconcat (lambda (k) k) keywords ", ")))

;;;###autoload
(defun my/denote-hook ()
  "Custom hook for denote."
  (push '(markdown-brh
          :extension ".md"
          :date-value-function denote-date-rfc3339
          :date-value-reverse-function denote-extract-date-from-front-matter
          :front-matter denote-yaml-front-matter
          :title-key-regexp "^title\\s-*:"
          :title-value-function denote-trim-whitespace
          :title-value-reverse-function denote-trim-whitespace
          :keywords-key-regexp "^tags\\s-*:"
          :keywords-value-function my/denote-format-keywords-for-md-front-matter
          :keywords-value-reverse-function denote-extract-keywords-from-front-matter
          :link denote-md-link-format
          :link-in-context-regexp denote-md-link-in-context-regexp)
        denote-file-types)
  (setq denote-directory (expand-file-name "~/Documents/notes/")
        denote-file-type 'markdown-brh
        denote-rename-buffer-mode 1
        denote-sort-keywords t))

(provide 'my-notes)

;;; my-notes.el ends here.
