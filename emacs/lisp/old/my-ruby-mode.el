;; (require 'ruby-mode)
(require 'font-lock)

(font-lock-add-keywords 'ruby-mode '(("[][(){}]" . font-lock-brace-face)))

(defun my-ruby-insert-block-comment ()
  (interactive)
  (my-insert-block-comment 'ruby-reindent-then-newline-and-indent "#" "#" "#"))

(defun my-ruby-mode-hook ()
  (auto-fill-mode)
  (make-local-variable 'adaptive-fill-regexp)
  (setq indent-tabs-mode nil
	;; ruby-indent-level 4
	adaptive-fill-regexp "^[	 ]*# "
	)
  (font-lock-mode t)
  (show-paren-mode t)
  (local-set-key [(meta control \;)] 'my-ruby-insert-block-comment)
  )
