;;; my-env.el --- my environment constants -*- lexical-binding: t; -*-
;;; Commentary:
;;;
;;; These are constants that are fixed and know at Emacs start up time. There are some that are not known until the
;;; `init.el' file is being loaded -- those are found in `my-constants.el`.
;;;
;;; Code:

(require 'info)
(require 'my-constants)
(require 'my-functions)

;;;###autoload
(defun my/is-terminal ()
  "T if running in a terminal.
NOTE: can return false positives if called too early in startup."
  (not (display-graphic-p)))

;;;###autoload
(defun my/is-x-windows ()
  "T if running in an X windows environment.
NOTE: can return false positives if called too early in startup."
  (eq window-system 'x))

;;;###autoload
(defalias 'my/is-graphical #'display-graphic-p
  "T if running in a graphical display environment.
NOTE: can return false positives if called too early in startup.")

;;;###autoload
(defun my/is-x-windows-on-win ()
  "T if running in VcXsrv on Windows.
Hacky but for now it works since we are always starting up an initial xterm."
  (and (my/is-x-windows) (getenv "XTERM_SHELL")))

;;;###autoload
(defun my/add-trusted-content-directory (path)
  "Add PATH to `trusted-content'."
  (push (abbreviate-file-name (file-name-as-directory path)) trusted-content))

;;;###autoload
(defun my/repos ()
  "LOCATION of root of personal source git repositories."
  (file-name-as-directory (file-truename "~/src/Mine")))

;;;###autoload
(defun my/init-files ()
  "Location of configurations repo."
  (file-name-as-directory (file-name-concat (my/repos) "init-files")))

;;;###autoload
(defun my/emacs.d ()
  "Location of emacs.d directory in the configurations repo.
Note that this is *not* the `user-emacs-directory', but rather the
location in the git repo where personal files are kept under version
control."
  (file-name-as-directory (file-name-concat (my/init-files) "emacs.d")))

;;;###autoload
(defun my/user-lisp ()
  "Location of personal Emacs Lisp files."
  (file-name-as-directory (file-name-concat (my/emacs.d) "user-lisp")))

;;;###autoload
(defun my/tmp-dir ()
  "The directory to use for temporary purposes - usually $HOME/tmp.
Creates the directory if it does not exist."
  (let* ((tmp (file-truename "~/tmp")))
    (unless (my/is-valid-directory tmp)
      (make-directory tmp t))
    (file-name-as-directory tmp)))

;;;###autoload
(defun my/venv ()
  "The Python virtual environment to use for eglot."
  (file-name-as-directory (file-truename "~/venv")))

;;;###autoload
(defun my/venv-python ()
  "The path to the Python executable to use for eglot."
  (file-name-concat (my/venv) "bin/python"))

;; (message "Info-default-directory-list: %s" Info-default-directory-list)

;;;###autoload
(defun my/env-setup ()
  "Setup Emacs to utilize current environment."
  (setenv "WORKON_HOME" (my/venv))
  (let* ((common-paths (list (file-truename "~/bin")
                             (file-name-concat (my/venv) "bin")))
         (macosx-paths (if my/is-macosx
                           (list "/opt/homebrew/sqlite/bin"
                                 "/opt/homebrew/opt/grep/libexec/gnubin"
                                 "/opt/homebrew/bin")
                         '()))
         ;; Collection of valid 'bin' paths
         (bin-paths (seq-filter #'file-directory-p (append common-paths macosx-paths)))
       ;; Collection of parent paths from the `bin' paths (valid because the children are)
       (root-paths (mapcar #'file-name-parent-directory bin-paths))
       ;; Collection of valid `info' paths
       (info-paths (append (seq-filter #'file-directory-p (mapcar (lambda (p) (file-name-concat p "info")) root-paths))
                           (seq-filter #'file-directory-p (mapcar (lambda (p) (file-name-concat p "share/info")) root-paths))))
       ;; Collection of valid `man' paths
       (man-paths (append (seq-filter #'file-directory-p (mapcar (lambda (p) (file-name-concat p "man")) root-paths))
                          (seq-filter #'file-directory-p (mapcar (lambda (p) (file-name-concat p "share/man")) root-paths)))))
  ;; Set exec-path to contain the above paths
  (setq exec-path (append bin-paths exec-path))
  ;; (message "exec-path: %s" exec-path)
  (setq Info-additional-directory-list '("/opt/homebrew/share/info"))
  (setq Info-default-directory-list (append info-paths Info-default-directory-list))
  ;; (message "Info-default-directory-list: %s" Info-default-directory-list)

  (my/add-trusted-content-directory (abbreviate-file-name (my/init-files)))
  (push (abbreviate-file-name (my/emacs.d)) trusted-content)
  (push (abbreviate-file-name (file-name-as-directory (file-name-concat (my/init-files) "shell/"))) trusted-content)
  (push (abbreviate-file-name (my/user-lisp)) trusted-content)
  (push "/Applications/Emacs.app/Contents/Resources/lisp/" trusted-content)

  ;; (unless (null Info-directory-list)
  ;;   (setq Info-directory-list (append Info-default-directory-list Info-directory-list)))
  ;; Same for PATH environment variable
  (setenv "PATH" (concat (string-join bin-paths ":") ":" (getenv "PATH")))
  (setenv "INFOPATH" (concat (string-join info-paths ":") ":" (getenv "INFOPATH")))
  (setenv "MANPATH" (concat (string-join man-paths ":") ":" (getenv "MANPATH")))))

(provide 'my-env)

;;; my-env.el ends here.
