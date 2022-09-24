;;; emacs-format-file --- Summary:
;;; Commentary:
;;; Code:

(defun emacs-format-function ()
  "Format the buffer."
  (untabify (point-min) (point-max))
  (save-buffer))

(provide 'emacs-format-file)
;;; emacs-format-file.el ends here
