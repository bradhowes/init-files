;;; my-git.el --- git functions -*- lexical-binding: t; -*-
;;; -----1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2------
;;; Commentary:
;;; Code:

(require 'my-env)

(defcustom my/git-sync-buffer-name " *my/git-sync*"
  "The name of the buffer to use to hold output from my/git-sync func."
  :type '(string)
  :group 'my/customizations)

;;;###autoload
(defun my/git-sync (host path)
  "Execute a git pull on HOST in PATH."
  (interactive)
  (message "Running git-pull on %s:%s..." host path)
  (let* ((git (list "cd" path "&&"
                    "git" "stash" "push" "&&"
                    "git" "pull" "&&"
                    "git" "checkout" "'stash@{0}'" "emacs.d/places" "emacs.d/recentf" "&&"
                    "git" "stash" "drop"))
         (cmd (if host
                  (append (list "/usr/bin/ssh" "-tt" host) git)
                (append '("bash") git)))
         (name (concat "<" (or host "localhost") "|" path ">"))
         (args (append (list name my/git-sync-buffer-name) cmd))
         (proc (apply 'start-process args)))
    (add-function :around (process-filter proc)
                  (lambda (filt proc content)
                    (funcall filt proc (concat (process-name proc) ": " content))))
    proc))

;;;###autoload
(defun my/all-git-sync ()
  "Sync the configurations repo found in various locations at work."
  (interactive)
  (when-let* ((buf (get-buffer my/git-sync-buffer-name)))
    (kill-buffer buf))
  (let ((local (file-name-concat (my/repos) "configurations"))
        (home (file-truename "~/configurations")))
    ;; NOTE: treat `(nil local)` as the master and only update via magit
    (dolist (cfg (list (cons nil home)  ; /lxhome/howesbra/configurations
                       (cons "ldzls2164i" home)  ; vnc
                       (cons "ldzls2164i" local) ; vnc
                       (cons "nyzls1514n" local) ; internal ogsd / pickaxe
                       (cons "nyzls1644q" local) ; wolverine QA
                       (cons "nyzls1646q" local) ; raze QA
                       (cons "nyzls2686q" local) ; tcs QA
                       (cons "nyzls105i" local)))  ; logs archive
      (my/git-sync (car cfg) (cdr cfg))))
  (display-buffer my/git-sync-buffer-name))

(provide 'my-git)

;;; my-git.el ends here.
