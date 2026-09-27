;;; package -- my-js2-mode -*- Mode: Emacs-Lisp; lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package js2-mode
  :commands (js-send-last-sexp js-send-buffer)
  :defines (js2-basic-offset)
  :ensure t)

(require 'font-lock)
(require 'my-insert-block-comment)
(require 'my-fontify-braces)

;;local
(defun my/js2-mode-hook ()
  "Custom JS2 mode hook."
  (setq js2-basic-offset 2)
        ;; js2-enter-indents-newline t)

  (keymap-local-set "C-x C-e" #'js-send-last-sexp)
  (keymap-local-set "C-c b" #'js-send-buffer)

  (my/fontify-braces)
  (font-lock-mode t)
  (show-paren-mode t))

(provide 'my-js2-mode)

;;; my-js2-mode.el ends here
