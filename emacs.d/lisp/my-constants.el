;;; package -- my-constants -*- Mode: Emacs-Lisp; lexical-binding: t;-*-
;;; Commentary:
;;; Runtime constants that describe various properties of the environment.
;;; Code:

(require 'my-env)

;; NOTE: do not evaluate from within early-init.el -- it is too early for `display-graphic-p' to return a meaningful
;; value.
(defconst my/is-terminal
  (not (display-graphic-p))
  "T if running in a terminal.")

;; NOTE: do not evaluate from within early-init.el -- it is too early for `window-system' to return a meaningful value.
(defconst my/is-x-windows
  (eq window-system 'x)
  "T if running in an X windows environment.")

(defconst my/is-x-windows-on-win
  (and my/is-x-windows (getenv "XTERM_SHELL"))
  "T if running in VcXsrv on Windows.
Hacky but for now it works since we are always starting up an initial xterm.")

(defconst my/workspace-name
  (or (getenv "WORKSPACE_NAME") "N/A")
  "The value of WORKSPACE_NAME environment variable.")

(defconst my/font-name
  "Berkeley Mono"
  "The name of the font to use.")

(defconst my/dev-tmp
  "/apps/home/howesbra/tmp"
  "The directory to use for temporary files.")

(defun my/is-valid-directory (dir)
  "Check if DIR is valid, returning it if so or nil if not.
Note that `file-directory-p' returns t if the (string) length of DIR is
zero (0), so we detect that and report that as false."
  (and (file-directory-p dir)
       (> (length dir) 0)
       dir))

(defconst my/tmp-dir
  (let* ((work-tmp (my/is-valid-directory my/dev-tmp))
         (home-tmp (file-truename "~/tmp"))
         (tmp (or work-tmp home-tmp)))
    (unless (my/is-valid-directory tmp)
      (make-directory tmp t))
    (file-name-as-directory tmp))
  "The directory to use for temporary purposes - usually $HOME/tmp.
Creates the directory if it does not exist.")

(provide 'my-constants)

;;; my-constants.el ends here
