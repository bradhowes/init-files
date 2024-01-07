;;; package -- my-swift-mode -*- Mode: Emacs-Lisp -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'font-lock))

(font-lock-add-keywords 'swift-mode '(("[][(){}]" . font-lock-brace-face)))

(defun my/swift-mode-hook ()
  "Custom Swift mode."
  (font-lock-mode t)
  (show-paren-mode t))

(provide 'my-swift-mode)
;;; my-swift-mode.el ends here
