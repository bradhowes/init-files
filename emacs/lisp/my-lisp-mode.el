;;; package -- my-lisp-mode -*- Mode: Emacs-Lisp; lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'font-lock)
(require 'key-chord)
(require 'my-insert-block-comment)

(dolist (elt '(lisp-mode lisp-data-mode scheme-mode emacs-lisp-mode))
  (font-lock-add-keywords elt '(("[][(){}]" . font-lock-brace-face))))

;; (font-lock-add-keywords 'lisp-mode '(("[][(){}]" . font-lock-brace-face)))
;; (font-lock-add-keywords 'lisp-data-mode '(("[][(){}]" . font-lock-brace-face)))
;; (font-lock-add-keywords 'scheme-mode '(("[][(){}]" . font-lock-brace-face)))
;; (font-lock-add-keywords 'emacs-lisp-mode '(("[][(){}]" . font-lock-brace-face)))

(defun my/lisp-insert-block-comment ()
  "Insert block comment."
  (interactive)
  (my/insert-block-comment #'newline-and-indent ";;" ";;" ";;"))

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
  (key-chord-define-local "cc" #'my/noisy-check-parens)
  (font-lock-mode t)
  (show-paren-mode t)
  (local-set-key [(control x)(control e)] #'my/eval-this-defun)
  (local-set-key [(control x)(meta e)] #'eval-last-sexp)
  (local-set-key [(meta control \;)] #'my/lisp-insert-block-comment)
  (local-set-key [(control return)] #'eval-print-last-sexp))

(provide 'my-lisp-mode)
;;; my-lisp-mode.el ends here
