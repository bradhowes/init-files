;;; package -- my-ksh-mode -*- Mode: Emacs-Lisp -*-
;;; Commentary:
;;; Code:

(require 'my-insert-block-comment)

(defun my/ksh-insert-block-comment ()
  "Insert three indented comment lines.
Place cursor at end of middle line."
  (interactive)
  (my/insert-block-comment 'reindent-then-newline-and-indent "#" "#" "#"))

(defun my/ksh-mode-hook ()
  "Custom KSH mode hook."
  ;; (setq ksh-indent 4
  ;; 	ksh-align-to-keyword nil)
  (auto-fill-mode t)
  (font-lock-mode t)
  (show-paren-mode t)
  ;; (local-set-key [(return)] 'reindent-then-newline-and-indent)
  ;; (local-set-key [(f7)] 'compile)
  (local-set-key [(meta control \;)] #'my/ksh-insert-block-comment))

(provide 'my-ksh-mode)
;;; my-ksh-mode.el ends here
