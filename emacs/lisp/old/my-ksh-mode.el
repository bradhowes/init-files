;;; package -- my-ksh-mode -*- Mode: Emacs-Lisp -*-
;;; Commentary:
;;; Code:

(require 'my-insert-block-comment)

(defun my-ksh-insert-block-comment ()
  "Insert three indented comment lines.
Place cursor at end of middle line."
  (interactive)
  (my-insert-block-comment 'reindent-then-newline-and-indent "#" "#" "#"))

(defun my-ksh-mode-hook ()
  "Custom KSH mode hook."
  (auto-fill-mode t)
  (font-lock-mode t)
  (show-paren-mode t))

(provide 'my-ksh-mode)
;;; my-ksh-mode.el ends here
