;;; my-env.el --- my environment constants -*- lexical-binding: t; -*-
;;; Commentary:
;;;
;;; These are constants that are fixed and know at Emacs start up time. There are some that are not known until the
;;; `init.el' file is being loaded -- those are found in `my-constants.el`.
;;;
;;; Code:

(defconst my/is-macosx
  (eq system-type 'darwin)
  "T if running on macOS.
Note that this is also true when running in a terminal window.")

(defconst my/is-linux
  (eq system-type 'gnu/linux)
  "T if running on GNU/Linux system.
Note that this is also true when running in a terminal window.")

(defconst my/repos
  (file-name-as-directory (if my/is-macosx
                              (file-truename "~/src/Mine")
                            (file-truename "/apps/home/howesbra/repos")))
  "Location of root of personal source git repositories.")

(defconst my/configurations
  (let ((repo-cfg (file-name-as-directory (file-name-concat my/repos "configurations")))
        (home-cfg (file-name-as-directory (file-truename "~/configurations"))))
    (if (file-directory-p repo-cfg)
        repo-cfg
      home-cfg))
  "Location of configurations repo.")

(defconst my/is-work
  (or (string= "howesbra" user-login-name)
      (string= "sp_qa" user-login-name))
  "This is t if running at work.")

(defconst my/is-dev
  (and my/is-work (string-suffix-p "d" (system-name)))
  "T if running on dev box at work.")

(defconst my/is-qa
  (and my/is-work (string-suffix-p "q" (system-name)))
  "T if running on QA box at work.")

(defconst my/emacs.d
  (if my/is-qa
      (file-name-as-directory (file-truename "~/.emacs.d"))
    (file-name-as-directory (file-name-concat my/configurations "emacs.d")))
  "Location of emacs.d directory in the configurations repo.
Note that this is *not* the `user-emacs-directory', but rather the
location in the git repo where personal files are kept under version
control.")

(defconst my/lisp
  (file-name-as-directory (file-name-concat my/emacs.d "lisp"))
  "Location of personal Emacs Lisp files.")

(defconst my/venv
  (file-truename "~/venv")
  "The Python virtual environment to use for eglot.")

(setenv "WORKON_HOME" my/venv)

(defconst my/venv-python
  (file-name-concat my/venv "bin/python")
  "The path to the Python executable to use for eglot.")

(provide 'my-env)

;;; my-env.el ends here.
