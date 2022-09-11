(require 'font-lock)

(font-lock-add-keywords 'makefile-mode '(("[][(){}]" .
					  font-lock-brace-face)))


(defun my-makefile-insert-block-comment ()
  (interactive)
  (my-insert-block-comment 'newline-and-indent "#" "#" "#"))

(defun my-makefile-mode-hook ()
  (font-lock-mode t)
  (auto-fill-mode)
  (setq tab-width 4)
  (local-set-key [(return)] 'newline-and-indent)
  (local-set-key [(meta control \;)] 'my-makefile-insert-block-comment)
  (show-paren-mode t))
