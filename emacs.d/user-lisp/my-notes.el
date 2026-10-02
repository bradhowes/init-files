;;; my-notes.el ---  -*- lexical-binding: t; -*-
;;; -----1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2------
;;; Commentary:
;;; Code:

(require 'denote)
(require 'consult-notes)

;;;###autoload
(defun my/denote-hook ()
  "Custom hook for denote."
  (push '(markdown-brh
          :extension ".md"
          :date-function (lambda (date) (format-time-string "%F %T"))
          :front-matter denote-yaml-front-matter
          :title-key-regexp "^title\\s-*:"
          :title-value-function denote-trim-whitespace
          :title-value-reverse-function denote-trim-whitespace
          :keywords-key-regexp "^tags\\s-*:"
          :keywords-value-function my/denote-format-keywords-for-md-front-matter
          :keywords-value-reverse-function denote-extract-keywords-from-front-matter
          :link denote-md-link-format
          :link-in-context-regexp denote-md-link-in-context-regexp)
        denote-file-types))

(provide 'my-notes)

;;; my-notes.el ends here.
