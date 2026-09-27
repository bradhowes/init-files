;;; package -- my-makefile-mode -*- Mode: Emacs-Lisp; lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'font-lock)
(require 'my-insert-block-comment)
(require 'my-fontify-braces)

(defun my/makefile-insert-block-comment ()
  "Insert block comment."
  (interactive)
  (my/insert-block-comment #'newline-and-indent "#" nil "#"))

(defun my/makefile-mode-hook ()
  "Custom Makefile mode."
  (my/fontify-braces)
  (font-lock-mode t)
  (auto-fill-mode)
  (setq tab-width 4)
  (keymap-local-set "C-M-;" #'my/makefile-insert-block-comment)
  (show-paren-mode t))

(provide 'my-makefile-mode)

;;; my-makefile-mode.el ends here
