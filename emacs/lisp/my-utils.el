;;; package -- my-utils -*- Mode: Emacs-Lisp; lexical-binding: t; -*-
;;; Commentary:
;;; Misc functions
;;; Code:

(require 'crux)
(require 'emacs-pager)

;; My own version of some `crux` routines that use `find-file` instead of `find-file-other-window`
(defun my/find-user-init-file ()
  "Edit the `user-init-file`."
  (interactive)
  (find-file user-init-file))

(defun my/find-user-custom-file ()
  "Edit the `custom-file` if it exists."
  (interactive)
  (if custom-file
      (find-file custom-file)
    (message "No custom file defined.")))

(defun my/find-shell-init-file()
  "Edit a shell init file."
  (interactive)
  (let* ((shell (file-name-nondirectory (getenv "SHELL")))
         (shell-init-file (cond
                           ((string= "zsh" shell) crux-shell-zsh-init-files)
                           ((string= "bash" shell) crux-shell-bash-init-files)
                           ((string= "tcsh" shell) crux-shell-tcsh-init-files)
                           ((string= "fish" shell) crux-shell-fish-init-files)
                           ((string-prefix-p "ksh" shell) crux-shell-ksh-init-files)
                           (t (error "Unknown shell"))))
         (candidates (cl-remove-if-not 'file-exists-p (mapcar #'substitute-in-file-name shell-init-file))))
    (if (> (length candidates) 1)
        (find-file (completing-read "Choose shell init file: " candidates))
      (find-file (car candidates)))))

(defun my/dump-hashtable (hashtable)
  "Show the contents of HASHTABLE."
  (interactive "Xhash table: ")
  (when (hash-table-p hashtable)
    (let ((tmp (get-buffer-create "*dump*")))
      (switch-to-buffer-other-window tmp)
      (erase-buffer)
      (maphash (lambda (key value)
                 (insert key " -> " value "\n")) hashtable)
      (emacs-pager-mode))))

(provide 'my-utils)
;;; my-utils.el ends here
