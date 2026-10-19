;;; package -- my-swift-mode -*- Mode: Emacs-Lisp; lexical-binding: t; -*-
;;; -----1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2------
;;; Commentary:
;;; Code:

(require 'my-fontify-braces)
(require 'font-lock)

(use-package swift-mode
  :ensure t)

;;;###autoload
(defun my/swift-mode-hook ()
  "Custom Swift mode."
  (setq swift-mode:basic-offset 2)
  (my/fontify-braces)
  (font-lock-mode t)
  (show-paren-mode t))

(provide 'my-swift-mode)
;;; my-swift-mode.el ends here
