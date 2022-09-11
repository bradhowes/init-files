(require 'font-lock)

(font-lock-add-keywords 'sh-mode
			'(("${*\\([A-Za-z0-9_]+\\)}*" 1
			   font-lock-variable-name-face t)
			  ("[][(){}$]" 0 font-lock-brace-face t)))

(defun my-sh-insert-block-comment ()
  (interactive)
  (my-insert-block-comment 'newline-and-indent "#" "#" "#"))

(defun my-sh-mode-hook ()
  (font-lock-mode t)
  (auto-fill-mode 1)
  (show-paren-mode t)
  (local-set-key [(return)] 'newline-and-indent)
  (local-set-key [(meta control \;)] 'my-sh-insert-block-comment))
