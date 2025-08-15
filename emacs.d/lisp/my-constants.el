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

;; NOTE: do not evaluate from within early-init.el -- it is too early for `display-graphic-p' to return a meaningful
;; value.
(defconst my/is-x-windows
  (eq window-system 'x)
  "T if running in an X windows environment.")

(defconst my/is-x-windows-on-win (and my/is-x-windows (getenv "XTERM_SHELL"))
  "T if running in VcXsrv on Windows.
Hacky but for now it works since we are always starting up an initial xterm.")

(defconst my/screen-laptop (intern "my/screen-laptop")
  "Symbol to indicate display is MacBook Pro 16\" laptop screen.")

(defconst my/screen-4k (intern "my/screen-4k")
  "Symbol to indicate display is 4K screen.")

(defconst my/screen-laptop-4k (intern "my/screen-laptop-4k")
  "Symbol to indicate display width is laptop and 1 4K screen.")

(defconst my/screen-4k-4k (intern "my/screen-4k-4k")
  "Symbol to indicate display width is 2 4K screens.")

(defconst my/screen-laptop-4k-4k (intern "my/screen-laptop-4k-4k")
  "Symbol to indicate display width is laptop and 2 4K screens.")

(defconst my/screen-terminal (intern "my/screen-terminal")
  "Symbol to indicate display is a terminal.")

(defconst my/laptop-screen-width 2056
  "MacBook Pro 16\" M1 screen width in pixels.")

(defconst my/4k-screen-width 3840
  "4K external display width in pixels.")

(defconst my/workspace-name (or (getenv "WORKSPACE_NAME") "N/A")
  "The value of WORKSPACE_NAME environment variable.")

(defconst my/font-name "Berkeley Mono"
  "The name of the font to use.")

(defconst my/is-dev (and my/is-work (string-suffix-p "d" (system-name)))
  "T if running on dev box at work.")

(defconst my/is-qa (and my/is-work (string-suffix-p "q" (system-name)))
  "T if running on QA box at work.")

(defconst my/dev-tmp "/apps/home/howesbra/tmp"
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
  "The directory to use for temporary purposes - usually $HOME/tmp.")

(provide 'my-constants)
;;; my-constants.el ends here
