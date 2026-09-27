;;; package -- my-json-mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'indent-bars)
(require 'font-lock)
(require 'my-fontify-braces)

(defun my/json-mode-hook ()
  "Custom JSON mode."
  (setq indent-bars-spacing-override 2
        json-ts-indent-offset 2)
  (indent-bars-mode t)
  (font-lock-mode t)
  (my/fontify-braces)
  (show-paren-mode t))

(provide 'my-json-mode)

;;; my-json-mode.el ends here
