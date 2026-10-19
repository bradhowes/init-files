;;; package -- my-ksh-mode -*- Mode: Emacs-Lisp; lexical-binding: t; -*-
;;; -----1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2------
;;; Commentary:
;;; Code:

(require 'my-insert-block-comment)

;;;###autoload
(defun my/ksh-insert-block-comment ()
  "Insert three indented comment lines.
Place cursor at end of middle line."
  (interactive)
  (my/insert-block-comment 'reindent-then-newline-and-indent "#" "" "#"))

;;;###autoload
(defun my/ksh-mode-hook ()
  "Custom KSH mode hook."
  (auto-fill-mode t)
  (font-lock-mode t)
  (show-paren-mode t)
  (keymap-local-set "C-M-;" #'my/ksh-insert-block-comment))

(provide 'my-ksh-mode)

;;; my-ksh-mode.el ends here
