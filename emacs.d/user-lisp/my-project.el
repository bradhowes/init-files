;;; my-project.el --- custom window/frame navigation routines -*- lexical-binding: t; -*-
;;; -----1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2------
;;; Commentary:
;;; Code:

(require 'project)

(defun my/project-root ()
  "Run `dired' on the current project root."
  (interactive)
  ;; `project-current-directory-override' is set by `project-switch-project'.
  (dired project-current-directory-override))

;;;###autoload
(defun my/show-project-menu ()
  "Show the menu that is shown when switching to a new project."
  (interactive)
  (call-interactively (project--switch-project-command)))

;;;###autoload
(defun my/project-initialize ()
  "Custom `project' init."
  (setopt project-mode-line 'non-remote
          project-switch-commands '((project-find-dir "Dired" ?d)
                                    (project-eshell "Eshell" ?e)
                                    (project-find-file "File" ?f)
                                    (magit-project-status "Magit" ?g)
                                    (project-find-regexp "Rg" ?r)
                                    (my/project-root "Root" ?t)
                                    (project-any-command "Other" ?z))))

(provide 'my-project)

;;; my-project.el ends here.
