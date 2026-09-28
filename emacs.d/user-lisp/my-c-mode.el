;;; package -- my-c-mode -*- Mode: Emacs-Lisp; lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'my-c-mode-common)
(require 'my-insert-block-comment)

;;;###autoload
(defun my/c-insert-block-comment ()
  "Insert block comment."
  (interactive)
  (my/insert-block-comment 'newline "/*" " *" " */"))

;;;###autoload
(defun my/c-mode-hook ()
  "C mode hook."
  (c-add-style "My C Style" my/c-style t)
  (local-set-key [(control meta \;)] #'my/c-insert-block-comment))

(provide 'my-c-mode)
;;; my-c-mode.el ends here
