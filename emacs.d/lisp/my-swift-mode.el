;;; package -- my-swift-mode -*- Mode: Emacs-Lisp; lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'my-fontify-braces)
(require 'font-lock)

(defun my/swift-mode-hook ()
  "Custom Swift mode."
  (my/fontify-braces)
  (font-lock-mode t)
  (show-paren-mode t))

(provide 'my-swift-mode)
;;; my-swift-mode.el ends here
