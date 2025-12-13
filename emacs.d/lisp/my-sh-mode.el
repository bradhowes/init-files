;;; package -- my-sh-mode  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'font-lock)
(require 'my-insert-block-comment)
(require 'my-fontify-braces)

(use-package flymake-shellcheck
  :commands flymake-shellcheck-load
  :init
  (add-hook 'sh-mode-hook 'flymake-shellcheck-load))

(defun my/sh-insert-block-comment ()
  "Insert block comment."
  (interactive)
  (my/insert-block-comment 'newline-and-indent "#" "#" "#"))

(defun my/sh-mode-hook ()
  "Custom SH mode."
  (flymake-mode t)
  (font-lock-mode t)
  (auto-fill-mode 1)
  (show-paren-mode t)
  (my/fontify-braces)
  (font-lock-add-keywords nil
			  '(("${*\\([A-Za-z0-9_]+\\)}*" 1
			     font-lock-variable-name-face t)))
  (keymap-local-set "C-M-;" #'my/sh-insert-block-comment))

(provide 'my-sh-mode)

;;; my-sh-mode.el ends here
