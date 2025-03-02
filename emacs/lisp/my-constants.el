;;; package -- my-constants -*- Mode: Emacs-Lisp; lexical-binding: t;-*-
;;; Commentary:
;;; Misc constants describing environment
;;; Code:

(require 'my-work)

(defgroup my/customizations nil
  "The customization group for my settings."
  :prefix "my/"
  :group 'local)

(defconst my/venv (file-truename "~/venv")
  "The Python virtual environment to use for elpy.")

(setenv "WORKON_HOME" my/venv)

(defconst my/venv-python (concat my/venv "/bin/python")
  "The path to the Python executable to use for elpy.")

(defconst my/is-macosx (eq system-type 'darwin)
  "T if running on macOS.
Note that this is also true when running in a terminal window.")

(defconst my/is-linux (eq system-type 'gnu/linux)
  "T if running on GNU/Linux system.
Note that this is also true when running in a terminal window.")

(defconst my/is-terminal (not (display-graphic-p))
  "T if running in a terminal.")

(defconst my/is-x-windows (eq window-system 'x)
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

(defconst my/workspace-name (or (getenv "WORKSPACE_NAME") "")
  "The value of WORKSPACE_NAME environment variable.")

(defconst my/font-name "Berkeley Mono"
  "The name of the font to use.")

(defconst my/tmp-dir
  (let* ((work-tmp (my/dev-tmp))
         (home-tmp (file-truename "~/tmp"))
         (tmp (or work-tmp home-tmp)))
    (when (not (file-directory-p tmp))
      (make-directory home-tmp t))
    tmp)
  "The directory to use for temporary purposes - usually $HOME/tmp.")

(defcustom my/screen-4k-pick 0
  "The 4K screen to use for Emacs frames."
  :type '(natnum)
  :options '(0 1)
  :group 'my/customizations)

(defcustom my/next-window-wrap-around t
  "Wrap around when moving to next window in `ace-window' list."
  :type '(boolean)
  :group 'my/customizations)

(provide 'my-constants)
;;; my-constants.el ends here
