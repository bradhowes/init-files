;;; package -- my-java-mode -*- Mode: Emacs-Lisp -*-
;;; Commentary:
;;; Code:

(require 'cc-mode)
(require 'font-lock)

(font-lock-add-keywords 'java-mode '(("[][(){}]" . font-lock-brace-face)))

(defun my-java-mode-hook ()
  "Custom Java mode hook."
  (setq c-default-style "stroustrup"
	c-backspace-function 'backward-delete-char
	c-basic-offset 4
	c-auto-newline t
	c-tab-always-indent nil)
  (c-toggle-auto-newline 1)
  (font-lock-mode t)
  (show-paren-mode t))

(provide 'my-java-mode)
;;; my-java-mode.el ends here
