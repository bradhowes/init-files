;;; package -- my-dired-mode -*- Mode: Emacs-Lisp; lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dired)

(defun my/ediff-marked-pair ()
  "Run `ediff-files' on a pair of files marked in `dired' buffer."
  (interactive)
  (let ((marked-files (dired-get-marked-files nil)))
    (if (not (length= marked-files 2))
        (message "mark exactly 2 files")
      (ediff-files (nth 0 marked-files)
                   (nth 1 marked-files)))))

(defun my/dired-mode-hook ()
  "Custom hook for `dired' mode."
  (keymap-local-set "=" #'my/ediff-marked-pair))

(provide 'my-dired-mode)
;;; my-dired-mode.el ends here
