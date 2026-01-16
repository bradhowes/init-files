;;; package -- my-lisp-mode -*- Mode: Emacs-Lisp; lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'font-lock)
(require 'my-insert-block-comment)
(require 'my-fontify-braces)
(require 'yasnippet)
(require 'yasnippet-snippets)

(defun my/lisp-insert-block-comment ()
  "Insert block comment."
  (interactive)
  (my/insert-block-comment #'newline-and-indent ";;" nil nil))

(defun my/eval-this-defun (arg)
  "Eval last sexp ARG."
  (interactive "P")
  (save-excursion
    (end-of-defun)
    (eval-last-sexp arg)))

(defun my/lisp-data-mode-hook ()
  "Custom Lisp-Data mode."
  (font-lock-mode t)
  (show-paren-mode t))

(defun my/noisy-check-parens ()
  "Execute `check-parens' and notify if OK."
  (interactive)
  (check-parens)
  (message "Ok"))

(defun my/lisp-mode-hook ()
  "Custom Lisp mode."
  (keymap-local-set "C-c p" #'my/noisy-check-parens)
  (my/fontify-braces)
  (font-lock-mode t)
  (show-paren-mode t)
  (yas-minor-mode 1)
  (keymap-local-set "C-x C-e" #'my/eval-this-defun)
  (keymap-local-set "C-x M-e" #'eval-last-sexp)
  (keymap-local-set "C-M-;" #'my/lisp-insert-block-comment)
  (keymap-local-set "C-<return>" #'eval-print-last-sexp)
  (which-function-mode 1))

(provide 'my-lisp-mode)
;;; my-lisp-mode.el ends here
