;;; package -- my-dired-mode -*- Mode: Emacs-Lisp; lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dired)
(require 'projectile)

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

  (projectile-update-mode-line)

  ;; When the directory is on a remote machine always keep the buffer in case the connection is slow.
  ;; Only necessary to do this when `dired-kill-when-opening-new-dired-buffer' is not `nil'.
  (when (and dired-kill-when-opening-new-dired-buffer
             (file-remote-p default-directory))
    (set (make-local-variable 'dired-kill-when-opening-new-dired-buffer) nil))

  (local-set-key [(?=)] #'my/ediff-marked-pair))

(provide 'my-dired-mode)
;;; my-dired-mode.el ends here
