;;; my-finders.el --- custom window/frame navigation routines -*- lexical-binding: t; -*-
;;; -----1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2------
;;; Commentary:
;;; Code:

(require 'crux)

;;;###autoload
(defun my/find-shell-init-file ()
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

;; My own version of some `crux` routines that use `find-file` instead of `find-file-other-window`
;;;###autoload
(defun my/find-user-init-file (arg)
  "Edit the `user-init-file` when ARG is nil.
Otherwise, edit the `early-init.el' file instead, creating it if
necessary."
  (interactive "P")
  (find-file (locate-user-emacs-file (if arg "early-init.el" user-init-file))))

;;;###autoload
(defun my/find-user-custom-file ()
  "Edit the `custom-file` if it exists."
  (interactive)
  (if custom-file
      (find-file custom-file)
    (message "No custom file defined.")))

(provide 'my-finders)

;;; my-finders.el ends here.
