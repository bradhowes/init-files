;;; package -- my-json-mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'indent-bars)
(require 'font-lock)
(require 'my-fontify-braces)

(defun my/json-mode-hook ()
  "Custom JSON mode."
  (when (string= "yagconf" (file-name-extension (buffer-file-name (current-buffer))))
    (setq indent-bars-spacing-override 2))
  (my/fontify-braces)
  (indent-bars-mode t)
  (font-lock-mode t)
  (show-paren-mode t))

(provide 'my-json-mode)

;;; my-json-mode.el ends here
