;;; package -- my-sh-mode -*- Mode: Emacs-Lisp -*-
;;; Commentary:
;;; Code:

(require 'font-lock)
(require 'my-insert-block-comment)

(font-lock-add-keywords 'sh-mode
			'(("${*\\([A-Za-z0-9_]+\\)}*" 1
			   font-lock-variable-name-face t)
			  ("[][(){}$]" 0 font-lock-brace-face t)))

(defun my-sh-insert-block-comment ()
  "Insert block comment."
  (interactive)
  (my-insert-block-comment 'newline-and-indent "#" "#" "#"))

(defun my-sh-mode-hook ()
  "Custom SH mode."
  (font-lock-mode t)
  (auto-fill-mode 1)
  (show-paren-mode t))

(provide 'my-sh-mode)
;;; my-sh-mode.el ends here
