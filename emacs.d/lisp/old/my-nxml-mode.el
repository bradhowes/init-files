(eval-when-compile
  (require 'nxml-mode))

(defun my-nxml-mode-hook ()
  (local-unset-key [(control c)(control f)])
  (local-set-key [(control c)(?f)] 'nxml-finish-element))
