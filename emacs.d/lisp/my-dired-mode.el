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

(defconst my/dired-exec-font-lock (list dired-re-exe '(".+" (dired-move-to-filename) nil (0 'sh-quoted-exec)))
  "Font-lock keyword specification to colorize executable in `dired' mode.")

(defconst my/dired-hidden-font-lock (list " \\.[^.]+$" '(".+" (dired-move-to-filename) nil (0 'dired-ignored)))
  "Font-lock keyword specification to colorize hidden files in `dired' mode.")

(defun my/dired-mode-hook ()
  "Custom hook for `dired' mode."

  ;; When the directory is on a remote machine always keep the buffer in case the connection is slow.
  ;; Only necessary to do this when `dired-kill-when-opening-new-dired-buffer` is set to `nil`.
  (when (and dired-kill-when-opening-new-dired-buffer
             (file-remote-p default-directory))
    (set (make-local-variable 'dired-kill-when-opening-new-dired-buffer) nil))

  ;; Colorize executable entries
  (unless (eq my/dired-exec-font-lock (car dired-font-lock-keywords))
    (push my/dired-exec-font-lock dired-font-lock-keywords))

  ;; Colorize `hidden' entries (first character is a '.')
  (unless (eq my/dired-hidden-font-lock (car dired-font-lock-keywords))
    (push my/dired-hidden-font-lock dired-font-lock-keywords))

  (keymap-local-unset "l")
  (keymap-local-set "=" #'my/ediff-marked-pair))

(provide 'my-dired-mode)
;;; my-dired-mode.el ends here
