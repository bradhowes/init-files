;;; package -- my-lisp-mode -*- Mode: Emacs-Lisp -*-
;;; Commentary:
;;; Code:

(require 'font-lock)
;; (require 'my-insert-block-comment)

(font-lock-add-keywords 'lisp-mode '(("[][(){}]" . font-lock-brace-face)))
(font-lock-add-keywords 'lisp-data-mode '(("[][(){}]" . font-lock-brace-face)))
(font-lock-add-keywords 'scheme-mode '(("[][(){}]" . font-lock-brace-face)))
(font-lock-add-keywords 'emacs-lisp-mode '(("[][(){}]" . font-lock-brace-face)))

;;(defun my-lisp-insert-block-comment ()
;;  "Insert block comment."
;;  (interactive)
;;  (my-insert-block-comment 'newline-and-indent ";;" ";;" ";;"))

(defun my-eval-last-sexp (arg)
  "Eval last sexp ARG."
  (interactive "P")
  (save-excursion
    (end-of-defun)
    (eval-last-sexp arg)))

(defun my-lisp-data-mode-hook ()
  "Custom Lisp-Data mode."
  (font-lock-mode t)
  (show-paren-mode t))

(defun my-lisp-mode-hook ()
  "Custom Lisp mode."
  (font-lock-mode t)
  (show-paren-mode t)
  (local-set-key [(control x)(control e)] 'my-eval-last-sexp)
  (local-set-key [(control return)] 'eval-print-last-sexp))

(provide 'my-lisp-mode)
;;; my-lisp-mode.el ends here
