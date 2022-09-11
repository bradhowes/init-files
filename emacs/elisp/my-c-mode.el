(require 'my-c-mode-common)

(defun my-c-insert-block-comment ()
  (interactive)
  (my-insert-block-comment 'newline "/*" " *" " */"))

(defun my-c-mode-hook ()
  (c-add-style "My C Style" my-c-style t)
  (local-set-key [(control meta \;)] 'my-c-insert-block-comment))
