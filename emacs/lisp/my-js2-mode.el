;;; package -- my-js2-mode -*- Mode: Emacs-Lisp -*-
;;; Commentary:
;;; Code:

(require 'js2-mode)
(require 'font-lock)
(require 'my-insert-block-comment)

(font-lock-add-keywords 'js2-mode '(("[][(){}]" . font-lock-brace-face)))

;;local
(defun my/js2-mode-hook ()
  "Custom JS2 mode hook."
  (setq js2-basic-offset 2)
        ;; js2-enter-indents-newline t)
  ;; (local-set-key [(control j)] 'newline)
  (local-set-key [(return)] #'my/cc-newline-and-indent)
  (local-set-key [(meta control \;)] #'my/cc-new-block-comment)

  (local-set-key (kbd "C-x C-e") #'js-send-last-sexp)
  (local-set-key (kbd "C-c b") #'js-send-buffer)
  (local-set-key (kbd "C-c C-b") #'js-send-buffer-and-go)

  (font-lock-mode t)
  (show-paren-mode t))

(provide 'my-js2-mode)

;;; my-js2-mode.el ends here
