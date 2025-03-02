;;; package -- my-work -*- Mode: Emacs-Lisp; lexical-binding: t; -*-
;;; Commentary:
;;; Misc functions / settings for work.
;;; Code:

;; (require 'my-constants)

(defconst my/is-work (or (string= "howesbra" user-login-name)
                         (string= "sp_qa" user-login-name))
  "This is t if running at work.")

(defconst my/repos (file-name-directory (file-truename "~/repos"))
  "Location of root of source repositories.")

(defconst my/is-dev (and my/is-work (string-suffix-p "d" (system-name)))
  "T if running on dev machine.")

(defconst my/is-qa (and my/is-work (string-suffix-p "q" (system-name)))
  "T if running on QA system.")

(defun my/dev-tmp ()
  "The directory to use for temporary purposes when in dev environment."
  (let ((dev-tmp "/apps/home/howesbra/tmp"))
    (and my/is-dev (file-directory-p dev-tmp)
         dev-tmp)))

(defun my/dired-raze ()
  "Open Dired on raze repo."
  (interactive)
  (dired (files--splice-dirname-file my/repos "raze")))

(defun my/dired-x23 ()
  "Open Dired on x23 repo."
  (interactive)
  (dired (files--splice-dirname-file my/repos "x23")))

(defun my/dired-tcs ()
  "Open Dired on tcs repo."
  (interactive)
  (dired (files--splice-dirname-file my/repos "tcs_hft")))

(defun my/dired-wolverine-config ()
  "Open Dired on wolverine-config repo."
  (interactive)
  (dired (files--splice-dirname-file my/repos "wolverine-config")))

(defun my/dired-wolverine-config-qa ()
  "Open Dired on x23 repo."
  (interactive)
  (dired (files--splice-dirname-file my/repos "wolverine-config-qa")))

(defun my/dired-yagbrat ()
  "Open Dired on yagbrat repo."
  (interactive)
  (dired (files--splice-dirname-file my/repos "yagbrat")))

(defun my/dired-qa-og ()
  "Open Dired on og log directory in qa."
  (interactive)
  (dired (file-truename "/apps/sp/logs/sp_qa/og")))

(defun my/dired-qa-tcs ()
  "Open Dired on tcs_hft log directory in qa."
  (interactive)
  (dired (file-truename "/apps/sp/logs/sp_qa/tcs_hft")))

;; Custom dir-locals
(dir-locals-set-class-variables 'raze-variables '((nil . ((compile-command . "./build.sh -m Debug")))))
(dir-locals-set-directory-class (file-truename "~/repos/raze") 'raze-variables)

(dir-locals-set-class-variables 'x23-variables '((nil . ((compile-command . "cmake -S . -B build && cd build && make tests ")))))
(dir-locals-set-directory-class (file-truename "~/repos/x23") 'x23-variables)

;; Jump to a directory -- "C-c j r" => dired buffer in Raze repo
(defvar my/dired-jumps-map (make-sparse-keymap)
  "Keymap for quick Dired jumps.")

(keymap-set my/dired-jumps-map "q" #'my/dired-qa-og)
(keymap-set my/dired-jumps-map "c" #'my/dired-qa-tcs)
(keymap-set my/dired-jumps-map "r" #'my/dired-raze)

;; Make "C-c j t" jump to tcs_hft log in QA but tcs_hft repo in DEV
(keymap-set my/dired-jumps-map "t" (if my/is-qa #'my/dired-qa-tcs #'my/dired-tcs))
(keymap-set my/dired-jumps-map "w" #'my/dired-wolverine-config)
(keymap-set my/dired-jumps-map "x" #'my/dired-x23)
(keymap-set mode-specific-map "j" my/dired-jumps-map)

;; Unfortunately I do not see a way to do this inside `use-package`. When I try I get failure to load magit.
(when my/is-work
  (unless (package-installed-p 'magit)
    (package-vc-install '(magit :url "https://github.com/magit/magit.git" branch: "v4.1.3" :lisp-dir "lisp"
                                :doc "docs/magit.texi"))))

(provide 'my-work)
;;; my-work.el ends here
