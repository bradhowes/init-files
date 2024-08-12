;;; package -- my-makefile-mode -*- Mode: Emacs-Lisp -*-
;;; Commentary:
;;; Code:

(require 'font-lock)
(require 'my-insert-block-comment)

(font-lock-add-keywords 'makefile-mode '(("[][(){}]" .
					  font-lock-brace-face)))


(defun my/makefile-insert-block-comment ()
  "Insert block comment."
  (interactive)
  (my/insert-block-comment #'newline-and-indent "#" "#" "#"))

(defun my/makefile-mode-hook ()
  "Custom Makefile mode."
  (font-lock-mode t)
  (auto-fill-mode)
  (setq tab-width 4)
  (local-set-key [(meta control \;)] #'my/makefile-insert-block-comment)
  (show-paren-mode t))

(provide 'my-makefile-mode)
;;; my-makefile-mode.el ends here
