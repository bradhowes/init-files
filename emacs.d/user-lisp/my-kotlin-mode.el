;;; package -- my-kotlin-mode -*- Mode: Emacs-Lisp; lexical-binding: t; -*-
;;; -----1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2------
;;; Commentary:
;;; Code:

(require 'eglot)

(use-package kotlin-ts-mode
  :ensure t)

(require 'font-lock)
(require 'my-fontify-braces)

;;;###autoload
(defun my/kotlin-mode-hook ()
  "Customize Kotlin mode."

  (eglot-ensure)
  (font-lock-mode t)
  (my/fontify-braces)
  (show-paren-mode t))

(provide 'my-kotlin-mode)

;;; my-kotlin-mode.el ends here
