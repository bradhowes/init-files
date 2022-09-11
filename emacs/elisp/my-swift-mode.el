(eval-when-compile
  (require 'font-lock))

(font-lock-add-keywords 'swift-mode '(("[][(){}]" . font-lock-brace-face)))

(defun my-swift-mode-hook ()
  (font-lock-mode t)
  (show-paren-mode t))
