;;; package -- my-dired-mode -*- Mode: Emacs-Lisp -*-
;;; Commentary:
;;; Code:

(require 'dired-aux)
(require 'dired-x)

(defun my-ediff-marked-pair ()
  "Run `ediff-files' on a pair of files marked in `dired' buffer."
  (interactive)
  (let ((marked-files (dired-get-marked-files nil)))
    (if (not (= (length marked-files) 2))
        (message "mark exactly 2 files")
      (ediff-files (nth 0 marked-files)
                   (nth 1 marked-files)))))

(defun my-dired-load-hook ()
  "Custom hook for `dired' loading."
  (load "dired-x")
  (global-set-key [(control x)(control j)] 'goto-line))

(defun my-dired-mode-hook ()
  "Custom hook for `dired' mode."
  (load "dired-x")
  (message "hi mom")
  ;; (dired-omit-mode 1)
  ;; (setq dired-omit-files "^\\.$\\||^\\.\\.$")
  (local-set-key [(?=)] 'my-ediff-marked-pair))

(provide 'my-dired-mode)
;;; my-dired-mode.el ends here
